#!/usr/bin/env nextflow
/*
========================================================================================
                                    BFTX-NF Pipeline
========================================================================================
*/

// DSL 2
nextflow.enable.dsl=2
version = '1.1'

// include helper functions
include { help_or_version; export_params_yaml; parse_beastgen_yaml; merge_params } from './lib/utilities'

// include workflows
include { run_nextclade_qc } from './lib/workflows/nextclade_qc'
include { usher_build_tree } from './lib/workflows/usher'
include { run_treetime_qc } from './lib/workflows/treetime_qc'
include { run_chron_qc } from './lib/workflows/chron_qc'

// help and version messages
help_or_version(version)

// write params summary to file
// export_params_yaml()

// main workflow
workflow {
    Channel
        .from(params.runs)
        .map({
            run_params = merge_params(params.global, it)
            return [it.key, run_params]
        })
        .set{ ch_runs }

    run_nextclade_qc(ch_runs)
    usher_build_tree(run_nextclade_qc.out.ch_qc_seqs)

    usher_build_tree.out.ch_uqc_tree_meta
        .filter { !it[1].usher.stop && it[1].temp_qc_mode == 1 }
        .join(usher_build_tree.out.ch_uqc_seqs_fa) \
        | run_treetime_qc

    usher_build_tree.out.ch_uqc_tree_meta
        .filter { !it[1].usher.stop && it[1].temp_qc_mode == 2 } \
        | run_chron_qc

}