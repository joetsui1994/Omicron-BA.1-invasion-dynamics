nextflow.enable.dsl=2

// cut up input metadata into batches, each containing sequences sampled on a given day
process split_batches {
    input:
        path(meta_tsv)
    output:
        file "*.tsv"
    """
    python3 $projectDir/lib/scripts/split_batches.py \
        --infile $meta_tsv
    """
}

// extract sequences from master fasta file for each batch individually
// randomly shuffle sequence order
process prepare_fasta {
    input:
        tuple val(batchID), path(batch_seqs_tsv)
    output:
        tuple val(batchID), path("${batchID}_seqs.shuffled.fasta")
    """
    seqkit grep -n -f $batch_seqs_tsv $params.masterFasta > "${batchID}_seqs.fasta"
    seqkit shuffle -2 "${batchID}_seqs.fasta" > "${batchID}_seqs.shuffled.fasta"
    """
}

// keep record of batches
process record_batches {
    input:
        tuple val(batchID), val(batch_shuffled_fa)
    output:
        val("")
    """
    echo -e "${batchID}\t${batch_shuffled_fa}" >> "${params.outDir}batches_record.tsv"
    """
}

// sort batches in chronological order
process sort_batches {
    input:
        val(trig)
    output:
        val("")
    """
    sort -k1 -n "${params.outDir}batches_record.tsv" -o "${params.outDir}batches_record.tsv"
    """
}

// (main) build tree with UShER
process build_usher {
    publishDir "${params.outDir}/usher_log", pattern: "*.usher.log", mode: "copy"
    publishDir "${params.outDir}", pattern: "*.fasta", mode: "copy"
    input:
        val(trig)
    output:
        file "*.usher.log"
        file "prev_excluded.fasta"
        path("uncondensed-final-tree.nh"), emit: usher_iter0_nh
        path("usher_iter0.pb"), emit: usher_iter0_pb
    """
    touch prev_excluded.fasta
    batchCount=0
    batchNum=\$(grep -c '.' "${params.outDir}batches_record.tsv")
    while read batch; do
        prevBatchCount=\$batchCount
        batchCount=\$((batchCount+1))
        
        batchID=\$(cut -d'\t' -f1 <<< \$batch)
        batchFaPath=\$(cut -d'\t' -f2 <<< \$batch)

        faToVcf <(cat $params.refFasta \$batchFaPath prev_excluded.fasta) "\${batchID}_prev_added.with_ref.vcf"
       
        if [ \$batchCount == 1 ]
        then
            usher -i "${params.startingPb}" -v "\${batchID}_prev_added.with_ref.vcf" -e ${params.eppMax} -o "\${batchCount}_out.pb" >> "\${batchID}.usher.log" 2>&1
        elif [ \$batchCount == \$batchNum ]
        then
            usher -i "\${prevBatchCount}_out.pb" -v "\${batchID}_prev_added.with_ref.vcf" -e ${params.eppMax} -u -o "\${batchCount}_out.pb" >> "\${batchID}.usher.log" 2>&1
        else
            usher -i "\${prevBatchCount}_out.pb" -v "\${batchID}_prev_added.with_ref.vcf" -e ${params.eppMax} -o "\${batchCount}_out.pb" >> "\${batchID}.usher.log" 2>&1
        fi
        
        awk -F '\t' '{ if (\$3 > ${params.eppMax}) {print \$1} }' placement_stats.tsv >> "\${batchCount}_excluded_seq_names.tsv"
        
        if [ -s "\${batchCount}_excluded_seq_names.tsv" ]
        then
            if [ -s "\${prevBatchCount}_excluded_seq_names.tsv" ]
            then
                if [ -n "\$(grep -F -x -v -f "\${prevBatchCount}_excluded_seq_names.tsv" "\${batchCount}_excluded_seq_names.tsv")" ]
                then
                    grep -F -x -v -f "\${prevBatchCount}_excluded_seq_names.tsv" "\${batchCount}_excluded_seq_names.tsv" > "\${batchCount}_newly_excluded.tsv"                
                    echo "(\${batchID}) newly excluded:" >> "${params.outDir}epp_record.tsv"
                    cat "\${batchCount}_newly_excluded.tsv" >> "${params.outDir}epp_record.tsv"
                fi

                if [ -n "\$(grep -F -x -v -f "\${batchCount}_excluded_seq_names.tsv" "\${prevBatchCount}_excluded_seq_names.tsv")" ]
                then
                    grep -F -x -v -f "\${batchCount}_excluded_seq_names.tsv" "\${prevBatchCount}_excluded_seq_names.tsv" > "\${batchCount}_newly_inserted.tsv"
                    echo "(\${batchID}) newly inserted:" >> "${params.outDir}epp_record.tsv"
                    cat "\${batchCount}_newly_inserted.tsv" >> "${params.outDir}epp_record.tsv"
                fi

                if [ -s "\${batchCount}_newly_excluded.tsv" ] | [ -s "\${batchCount}_newly_inserted.tsv" ]
                then
                    seqkit grep -n -f "\${batchCount}_excluded_seq_names.tsv" ${params.masterFasta} > prev_excluded.fasta
                fi
            else
                echo "(\${batchID}) newly excluded:" >> "${params.outDir}epp_record.tsv"
                cat "\${batchCount}_excluded_seq_names.tsv" >> "${params.outDir}epp_record.tsv"
                seqkit grep -n -f "\${batchCount}_excluded_seq_names.tsv" ${params.masterFasta} > prev_excluded.fasta
            fi
        else
            if [ -s "\${prevBatchCount}_excluded_seq_names.tsv" ]
            then
                echo "(\${batchID}) newly inserted:" >> "${params.outDir}epp_record.tsv"
                cat "\${prevBatchCount}_excluded_seq_names.tsv" >> "${params.outDir}epp_record.tsv"
                > prev_excluded.fasta
            fi
        fi
    
    done < "${params.outDir}batches_record.tsv"
    awk 'sub(/^>/, "")' prev_excluded.fasta > "${params.outDir}omitted_seq_names.tsv"

    mv "\${batchCount}_out.pb" usher_iter0.pb
    """
}

// resolve polytomies (random resolutions) using matUtils
process resolve_polytomies {
    publishDir "${params.outDir}", mode: "copy"
    input:
        path(tree_pb)
    output:
        file "*"
        path("tree.resolved.nh"), emit: tree_resolved_nh
    """
    matUtils extract -i $tree_pb -R -o tree.resolved.pb
    matUtils extract -i tree.resolved.pb -t tree.resolved.nh
    """
}

// reset branch length to root node to zero if not already
process reset_root_brlen {
    publishDir "${params.outDir}", mode: "copy"
    input:
        path(pre_root_brlen_reset_tree_nh)
    output:
        file "*"
        path("root_brlen_reset_tree.nh"), emit: root_brlen_reset_tree_nh
    """
    python3 $projectDir/lib/scripts/reset_root_brlen.py \
        --infile $pre_root_brlen_reset_tree_nh \
        --outfile root_brlen_reset_tree.nh \
        -d 0
    """
}

// extract alignment from master faster file given a tree
process extract_aln_from_tree {
    publishDir "${params.outDir}", mode: "copy"
    input:
        path(tree_nh)
    output:
        file "*"
        path("tree_seqs.fasta"), emit: tree_seqs_fa
    """
    gotree labels -i $tree_nh --tips > tree_seq_names.tsv
    seqkit grep -n -f tree_seq_names.tsv ${params.masterFasta} > tree_seqs.fasta
    """
}

// optimise iteration-0 tree from UShER using FastTreeMP
process fasttreeMP_optimise {
    publishDir "${params.outDir}/fasttreeMP_log", pattern: "*.fasttreeMP.log", mode: "copy"
    publishDir "${params.outDir}", pattern: "*.nh", mode: "copy"
    input:
        path(pre_fasttreeMP_seqs_fa)
        path(pre_fasttreeMP_tree_nh)
    output:
        file "fasttreeMP_optimised.nh"
        file "*.fasttreeMP.log"
        path("fasttreeMP_optimised.nh"), emit: fasttreeMP_optimised_nh
    """
    for iterCount in {1..$params.fasttreeMP.iterN}
    do
        currIter=\$iterCount
        ((iterCount--))

        if [ \$iterCount == 0 ]
        then
            starting_nh=$pre_fasttreeMP_tree_nh
        else
            starting_nh="iter\${iterCount}.fasttreeMP.nh"
        fi

        FastTree -nt -gamma -sprlength $params.fasttreeMP.sprlength -nni $params.fasttreeMP.nni -spr $params.fasttreeMP.spr -log "iter\${currIter}.fasttreeMP.log" -nosupport -intree \$starting_nh $pre_fasttreeMP_seqs_fa > "iter\${currIter}.fasttreeMP.nh"
    done

    mv "iter${params.fasttreeMP.iterN}.fasttreeMP.nh" fasttreeMP_optimised.nh
    """
}

// optimise iteration-0 tree from UShER using matOptimize
process matOptimize {
    publishDir "${params.outDir}/matOptimize_log", pattern: "*.matOptimize.log", mode: "copy"
    publishDir "${params.outDir}", pattern: "*.nh", mode: "copy"
    input:
        path(pre_matOptimize_seqs_fa)
        path(pre_matOptimize_tree_nh)
    output:
        file "final.matOptimize.nh"
        file "*.matOptimize.log"
        path("final.matOptimize.nh"), emit: matOptimized_nh
     """
    faToVcf <(cat $params.refFasta $pre_matOptimize_seqs_fa) pre_matOptimize_seqs.vcf
    usher -t $pre_matOptimize_tree_nh -v pre_matOptimize_seqs.vcf -c -o pre_matOptimize_tree.pb

    for iterCount in {1..$params.matOptimize.iterN}
    do
        currIter=\$iterCount
        ((iterCount--))

        if [ \$iterCount == 0 ]
        then
            starting_pb=pre_matOptimize_tree.pb
        else
            starting_pb="iter\${iterCount}.matOptimize.pb"
        fi

        matOptimize -i \$starting_pb -v pre_matOptimize_seqs.vcf -o iter\${currIter}.matOptimize.pb -r $params.matOptimize.radius -T 16 -s 259200 2>&1 | tee "iter\${currIter}.matOptimize.log" 
    done

    if [ $params.matOptimize.final_radius -gt 0 ]
    then
        matOptimize -i "iter${params.matOptimize.iterN}.matOptimize.pb" -v pre_matOptimize_seqs.vcf -o final.matOptimize.pb -r $params.matOptimize.final_radius -T 16 -s 259200 2>&1 | tee final.matOptimize.log 
    else
        mv "iter${params.matOptimize.iterN}.matOptimize.pb" final.matOptimize.pb
    fi

    matUtils extract -i final.matOptimize.pb -t final.matOptimize.nh
    """
}
