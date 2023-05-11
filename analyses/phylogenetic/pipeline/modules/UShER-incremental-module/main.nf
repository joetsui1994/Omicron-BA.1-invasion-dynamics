#!/usr/bin/env nextflow
/*
==================================================
                UShER Incremental
==================================================
*/

// DSL 2
nextflow.enable.dsl=2
version = '1.0'

// include helper functions
include { help_or_version; clean_outDir } from './lib/utilities'

// include single-step processes
include { split_batches; prepare_fasta; record_batches; sort_batches; build_usher; extract_aln_from_tree; reset_root_brlen; resolve_polytomies; fasttreeMP_optimise; matOptimize } from './lib/single_steps'

// main workflow
workflow {
    clean_outDir() // clean up output directory

    Channel
        .from(params.infile)
        .set{ ch_in }

    split_batches(ch_in)
    split_batches.out
        .map { file -> tuple(file.baseName, file) }
        .transpose() | prepare_fasta

    prepare_fasta.out | record_batches
    
    record_batches.out.collect() | sort_batches
    sort_batches.out.collect() | build_usher

    resolve_polytomies(build_usher.out.usher_iter0_pb)
    reset_root_brlen(resolve_polytomies.out.tree_resolved_nh)
    extract_aln_from_tree(reset_root_brlen.out.root_brlen_reset_tree_nh)

    if ( params.fasttreeMP.iterN > 0 ) {
        fasttreeMP_optimise(extract_aln_from_tree.out.tree_seqs_fa, reset_root_brlen.out.root_brlen_reset_tree_nh)
        if ( params.matOptimize.iterN > 0 ) {
            matOptimize(extract_aln_from_tree.out.tree_seqs_fa, fasttreeMP_optimise.out.fasttreeMP_optimised_nh)
        }
    }
    else {
        if ( params.matOptimize.iterN > 0 ) {
            matOptimize(extract_aln_from_tree.out.tree_seqs_fa, reset_root_brlen.out.root_brlen_reset_tree_nh)
        }
    }

}
