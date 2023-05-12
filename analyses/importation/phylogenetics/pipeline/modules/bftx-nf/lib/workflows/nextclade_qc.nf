nextflow.enable.dsl=2

// actually running of nextclade
process run_nextclade {
    publishDir "${run_params.outDir}/output_${key}/nextclade_qc", mode: "copy"

    input:
        tuple val(key), val(run_params)

    output:
        file "*"
        tuple val(key), val(run_params), path("nextclade.tsv"), emit: nextclade_out

    """
    nextclade \
        --in-order \
        --input-fasta ${run_params.inDir}/sequences.fasta \
        --input-root-seq ${run_params.nextclade_qc.refDir}/reference.fasta \
        --input-dataset ${run_params.nextclade_qc.refDir} \
        --input-qc-config ${run_params.nextclade_qc.qc_json} \
        --output-tsv nextclade.tsv \
        --output-tree nextclade.auspice.json \
        --output-dir . \
        --output-basename nextclade \
    """
}

// process nextclade output (specify QC logic here) and apply filtering to fasta and meta files
process process_nextclade {
    publishDir "${run_params.outDir}/output_${key}/nextclade_qc", mode: "copy"

    input:
        tuple val(key), val(run_params), path(qc_out)

    output:
        file "*"
        tuple val(key), val(run_params), path("qc_sequences.fasta"), emit: nextclade_qc_out

    """
    awk -F '\t' '{ if (\$4 == "good") print \$1 }' $qc_out > qc_names.tsv
    seqkit grep -n -f qc_names.tsv ${run_params.inDir}/sequences.fasta > qc_sequences.fasta
    awk -F '\t' -v OFS='\t' 'NR==FNR { a[\$1]; next } FNR==1 || \$1 in a { print \$0 }' qc_names.tsv ${run_params.inDir}/meta.tsv > qc_meta.tsv
    """
}

workflow run_nextclade_qc {
    take:
        ch_runs

    main:
        ch_runs \
            | run_nextclade

        run_nextclade.out.nextclade_out \
            | process_nextclade
            
    emit:
        ch_qc_seqs = \
            process_nextclade.out.nextclade_qc_out
                .filter { !it[1].nextclade_qc.stop }
}