#!/usr/bin/env bash

seqkit grep -n -f !{batch_seqs_tsv} !{params.masterFasta} > "!{batchID}_seqs.fasta"
seqkit shuffle -2 "!{batchID}_seqs.fasta" > "!{batchID}_seqs.shuffled.fasta"