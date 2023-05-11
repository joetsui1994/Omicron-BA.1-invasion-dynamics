nextflow.enable.dsl=2

// run chronumental
process run_chronumental {
    publishDir "${run_params.outDir}/output_${key}/chronumental", mode: "copy"

    input:
        tuple val(key), val(run_params), path(uqc_unrooted_nh), path(meta_tsv)

    output:
        file "*"
        tuple val(key), val(run_params), path("timetree.nh"), path("pred_dates.tsv"), emit: chon_out

    """
    DENORM_CLOCK=\$(expr $run_params.chron_qc.clock_rate*$run_params.seqLen | bc)
    chronumental \
        --tree $uqc_unrooted_nh \
        --dates $meta_tsv \
        --dates_out pred_dates.tsv \
        --tree_out timetree.nh \
        --clock \$DENORM_CLOCK \
        --variance_dates $run_params.chron_qc.var_dates \
        --steps $run_params.chron_qc.steps \
        --output_unit years
    """
}

// remove outliers according chronumental output
process remove_outliers {
    publishDir "${run_params.outDir}/output_${key}/chronumental", mode: "copy"

    input:
        tuple val(key), val(run_params), path(divergence_tree_nex), path(time_tree_nex)

    output:
        tuple val(key), val(run_params), path("refined_divergence_tree.nh"), path("refined_time_tree.nh")

    """
    python3 $projectDir/libs/scripts/chron_parse.py \
        --infile $pred_dates_tsv
        --outfile $pass_taxa.tsv
        --dates $meta_tsv
        --variance $run_params.chron_qc.date_var \
        --p_value $run_params.chron_qc.date_p_val

    
    """
}

// // resolve polytomies using fertree
// process resolve_tree {
//     publishDir "${run_params.outDir}/output_${key}/treetime", mode: "copy"

//     input:
//         tuple val(key), val(run_params), path(refined_divergence_tree), path(refined_time_tree)

//     output:
//         file "*"
//         // path("resolved_time_tree.nw"), emit: resolved_time_tree

//     """
//     RUST_LOG=info fertree resolve zero -I $refined_divergence_tree > resolved_divergence_tree.nw 2> divergence_resolve.log
//     RUST_LOG=info fertree resolve evenly -I $refined_time_tree > resolved_time_tree.nw 2> time_resolve.log
//     """
// }

workflow run_chron_qc {
    take:
        ch_usher_out

    main:
        run_chronumental(ch_usher_out)
}