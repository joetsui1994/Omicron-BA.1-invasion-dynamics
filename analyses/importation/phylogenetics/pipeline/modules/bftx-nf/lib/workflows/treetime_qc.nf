nextflow.enable.dsl=2

// run TreeTime
process run_treetime {
    label "treetime"
    publishDir "${run_params.outDir}/output_${key}/treetime", mode: "copy"

    input:
        tuple val(key), val(run_params), path(uqc_unrooted_nh), path(meta_tsv), path(uqc_seqs_fa)

    output:
        file "*"
        tuple val(key), val(run_params), path("outliers.tsv"), path("divergence_tree.nexus"), path("timetree.nexus"), emit: treetime_out

    """
    treetime \
        --tree $uqc_unrooted_nh \
        --dates $meta_tsv \
        --aln $uqc_seqs_fa \
        --name-column name \
        --date-column date \
        --clock-rate $run_params.treetime_qc.clock_rate \
        --clock-std-dev $run_params.treetime_qc.clock_rate_std \
        --clock-filter $run_params.treetime_qc.clock_filter \
        --keep-root \
        --keep-polytomies \
        --branch-length-mode $run_params.treetime_qc.brl_mode \
        --outdir ./

    awk -F '\t' '\$3=="--" { print \$1 }' dates.tsv > outliers.tsv
    """
}

// remove outliers according TreeTime output
process remove_outliers {
    label "gotree"
    publishDir "${run_params.outDir}/output_${key}/treetime", mode: "copy"

    input:
        tuple val(key), val(run_params), path(outliers_tsv), path(divergence_tree_nex), path(time_tree_nex)

    output:
        tuple val(key), val(run_params), path("refined_divergence_tree.nh"), path("refined_time_tree.nh")

    shell:
        template "remove_outliers.sh"
}

// resolve polytomies using fertree
process resolve_tree {
    publishDir "${run_params.outDir}/output_${key}/treetime", mode: "copy"

    input:
        tuple val(key), val(run_params), path(refined_divergence_tree), path(refined_time_tree)

    output:
        file "*"
        // path("resolved_time_tree.nw"), emit: resolved_time_tree

    """
    RUST_LOG=info fertree resolve zero -I $refined_divergence_tree > resolved_divergence_tree.nw 2> divergence_resolve.log
    RUST_LOG=info fertree resolve evenly -I $refined_time_tree > resolved_time_tree.nw 2> time_resolve.log
    """
}

workflow run_treetime_qc {
    take:
        ch_usher_out

    main:
        run_treetime(ch_usher_out)
        remove_outliers(run_treetime.out.treetime_out) | resolve_tree
}