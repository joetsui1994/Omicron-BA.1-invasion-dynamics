nextflow.enable.dsl=2

// convert fastas to vcfs
// build tree with Usher by inserting sequences into a single-tip starting tree
process build_tree {
    publishDir "${run_params.outDir}/output_${key}/usher", mode: "copy"

    input:
        tuple val(key), val(run_params), path(nqc_seqs_fa)

    output:
        file "*"
        tuple val(key), val(run_params), path("uncondensed-final-tree.nh"), path("placement_stats.tsv"), emit: iter0_out

    """
    faToVcf <(cat $run_params.usher.ref_seq_fa $nqc_seqs_fa) nqc.vcf
    faToVcf <(cat $run_params.usher.ref_seq_fa $run_params.usher.start_fa) start.vcf

    usher -t $run_params.usher.start_tree -v start.vcf -o start.pb
    usher -i start.pb -v nqc.vcf -u -o iter0.pb
    """
}

// prune tree by max_allowed_placements and extract filtered sequences and relevant metadata
// convert final newick tree to pb
process clean_tree {
    label "gotree"
    publishDir "${run_params.outDir}/output_${key}/usher", mode: "copy"

    input:
        tuple val(key), val(run_params), path(iter0_nh), path(placement_stats)

    output:
        tuple val(key), path("discard_placement_stats.tsv"), path("clean_iter0.nh")

    """
    awk -F '\t' '{ if (\$3 > $run_params.usher.max_placements_n) print \$0 }' $placement_stats > discard_placement_stats.tsv
    gotree prune -f <(cut -f1 discard_placement_stats.tsv) -i $iter0_nh > clean_iter0.nh
    """
}

// optimise final tree with matOptimize using two-step process
// first step runs for a larger number of iterations but uses a small SPR radius
// second step only runs for a single iteration but uses a larger SPR radius
process optimise_tree {
    publishDir "${run_params.outDir}/output_${key}/usher", mode: "copy"

    input:
        tuple val(key), val(run_params), path(nqc_seqs_fa), path(discard_placement_stats), path(clean_iter0_nh)

    output:
        file "*"
        tuple val(key), path("uqc_seqs.fasta"), emit: uqc_seqs_fa
        tuple val(key), val(run_params), path("final_optimised.nh"), emit: uqc_tree_nh

    """
    seqkit grep -n -v -f <(cut -f1 $discard_placement_stats) $nqc_seqs_fa > uqc_seqs.fasta
    faToVcf <(cat $run_params.usher.ref_seq_fa $run_params.usher.start_fa uqc_seqs.fasta) uqc_outgroup_merged.vcf
    usher -t $clean_iter0_nh -v uqc_outgroup_merged.vcf -c -o iter0.pb

    ITER_N=\$(( $run_params.usher.optimise_iter_n > 0 ? $run_params.usher.optimise_iter_n : 0))
    if [ \$ITER_N -gt 0 ]
    then
        for i in `seq 0 \$((\$ITER_N-1))`; do
            j=\$((i+1))
            matOptimize -i iter\${i}.pb -v uqc_outgroup_merged.vcf -o iter\${j}.pb -r $run_params.usher.optimise_radius -T 16 -s 259200 2>&1 | tee iter\${j}.log 
        done
    fi

    if [ $run_params.usher.optimise_final_radius -gt 0 ]
    then
        matOptimize -i iter\${ITER_N}.pb -v uqc_outgroup_merged.vcf -o final_optimised.pb -r $run_params.usher.optimise_final_radius -T 16 -s 259200 2>&1 | tee final_optimised.log 
    else
        mv iter\${ITER_N}.pb final_optimised.pb
    fi

    matUtils extract -i final_optimised.pb -t final_optimised.nh
    """
}

// reroot tree and remove outgroup
process reroot_tree {
    label "gotree"
    publishDir "${run_params.outDir}/output_${key}/usher", mode: "copy"

    input:
        tuple val(key), val(run_params), path(uqc_tree_nh)

    output:
        tuple val(key), val(run_params), path("uqc_unrooted.nh"), path("taxa.tsv")

    """
    gotree reroot outgroup "$run_params.usher.outgroup" -i $uqc_tree_nh --format newick --remove-outgroup | gotree unroot > uqc_unrooted.nh
    gotree labels -i uqc_unrooted.nh --tips > taxa.tsv
    """   
}

process extract_meta {
    input:
        tuple val(key), val(run_params), path(uqc_unrooted_nh), path(taxa_tsv)

    output:
        tuple val(key), val(run_params), path(uqc_unrooted_nh), path("meta.tsv")

    """
    awk -F '\t' -v OFS='\t' 'NR==FNR { a[\$1]; next } FNR==1 || \$1 in a { print \$0 }' $taxa_tsv $run_params.inDir/meta.tsv > meta.tsv
    """
}

workflow usher_build_tree {
    take:
        ch_qc_seqs

    main:
        build_tree(ch_qc_seqs)
        clean_tree(build_tree.out.iter0_out)

        ch_optimise = ch_qc_seqs.join(clean_tree.out)
        optimise_tree(ch_optimise)

        reroot_tree(optimise_tree.out.uqc_tree_nh) | extract_meta

    emit:
        ch_uqc_tree_meta = extract_meta.out
        ch_uqc_seqs_fa = optimise_tree.out.uqc_seqs_fa
}