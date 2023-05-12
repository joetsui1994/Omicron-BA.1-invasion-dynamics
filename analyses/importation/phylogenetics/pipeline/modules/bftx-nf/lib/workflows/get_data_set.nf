#!/usr/bin/env nextflow

nextflow.enable.dsl = 2

params.cog_quality_problems = "/cephfs/covid/bham/results/msa/latest/notifications/cog_date_problems.csv"
params.fasta = "/cephfs/covid/bham/raccoon-dog/datapipe-latest/publish_dev/cog_gisaid/cog_gisaid.fa"
params.outDir="./"


process initial_filter {
    /*
    filter based on quality checks. sequences that pass this filter will be used to caluclate weights.
    */

    input:
        path(metadata)
        path(cog_qualify)
    output:
        path "filtered_metadata.csv"
    script:
    """
        initial_filter.py ${metadata} ${cog_qualify} filtered_metadata.csv
    """

}

process calculate_weights {
    /*
    calculates weights for each ltla using seqeuncing coverage adapted from https://github.com/robj411/sequencing_coverage/
    */
    input:
        path quality_data
    output:
        path "weightsdf.csv"
  
    """
    Rscript ${workflow.projectDir}/scripts/sequencing_coverage/compute_weights.R --postcode ${workflow.projectDir}/scripts/sequencing_coverage/postcode_to_la.csv --days 14 --metadata $quality_data
    """
}

process final_filter {

    input:
        path metadata
    output:
       path  "metadata.csv"

    """
        xsv search -s scorpio_call "^Omicron \\(BA.[1,2]-like" $metadata  | xsv select sequence_name,cog_id,gisaid_id,scorpio_call,sample_date,published_date,country,adm1,is_pillar_2 > metadata.csv
    """
}

process extract_sequences {
    publishDir  "${params.outDir}/", pattern: "*.gz"

   input:
        path metadata
        path fasta
    output:
        path "cog_gisaid.gz"

    """
    xsv select  sequence_name $metadata >sequences.txt
    seqkit grep -f sequences.txt -o cog_gisaid.gz $fasta
    """
    
}

process finalize_metadata {
    publishDir  "${params.outDir}/", pattern: "*.csv"

    input:
    path metadata
    path weights

    output:
        "cog_gisaid_metadata.csv"
    """
    xsv join --left sequence_name $metadata sequence_name $weights > cog_gisaid_metadata.csv
    """

}
    metadata=file(params.metadata)
    cog_date_problems = file(params.cog_quality_problems)
workflow {
	
    
    initial_filter(metadata,cog_date_problems)
    final_filter(initial_filter.out)
    calculate_weights(initial_filter.out)
    fasta=file(params.fasta)
    extract_sequences(final_filter.out,fasta)
    finalize_metadata(final_filter.out,calculate_weights.out)

}
