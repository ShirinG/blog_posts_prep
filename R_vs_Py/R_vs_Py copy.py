#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 18 11:17:30 2017

@author: Shirin
"""

import pandas as pd

pd.__version__

df = pd.read_csv('../../Homo_sapiens.GRCh38.85.gff3.gz', compression = 'gzip',
                         sep = '\t', 
                         comment = '#', 
                         low_memory = False,
                         header = None, 
                         names = ['seqid', 'source', 'type', 'start', 'end', 'score', 'strand', 'phase', 'attributes'])

df.head()

df.info()

df.seqid.unique()
df['seqid'].unique()

df.seqid.unique().shape

df.source.value_counts()

gdf = df[df.source == 'GRCh38']
gdf.shape
gdf.sample(10)

gdf = gdf.copy()
gdf['length'] = gdf.end - gdf.start + 1

gdf.length.sum()

chrs = [str(_) for _ in range(1, 23)] + ['X', 'Y', 'MT']
gdf[-gdf.seqid.isin(chrs)].length.sum() / gdf.length.sum()

gdf[(gdf['type'] == 'supercontig')].length.sum() / gdf.length.sum()

edf = df[df.source.isin(['ensembl', 'havana', 'ensembl_havana'])]
edf.info()
edf.shape
edf.sample(10)

ndf = edf[edf.type == 'gene']
ndf = ndf.copy()
ndf.sample(10).attributes.values
ndf.shape

import re

RE_GENE_NAME = re.compile(r'Name=(?P<gene_name>.+?);')
def extract_gene_name(attributes_str):
    res = RE_GENE_NAME.search(attributes_str)
    return res.group('gene_name')


ndf['gene_name'] = ndf.attributes.apply(extract_gene_name)

RE_GENE_ID = re.compile(r'gene_id=(?P<gene_id>ENSG.+?);')
def extract_gene_id(attributes_str):
    res = RE_GENE_ID.search(attributes_str)
    return res.group('gene_id')


ndf['gene_id'] = ndf.attributes.apply(extract_gene_id)


RE_DESC = re.compile('description=(?P<desc>.+?);')
def extract_description(attributes_str):
    res = RE_DESC.search(attributes_str)
    if res is None:
        return ''
    else:
        return res.group('desc')


ndf['desc'] = ndf.attributes.apply(extract_description)
ndf.drop('attributes', axis=1, inplace=True)
ndf.head()

ndf.shape
ndf.gene_id.unique().shape
ndf.gene_name.unique().shape

ndf['length'] = ndf.end - ndf.start + 1
ndf.length.describe()

import matplotlib as plt

ndf.length.plot(kind='hist', bins=50, logy=True)
plt.show()

ndf[ndf.length > 2e6].sort_values('length').ix[::-1]

ndf.sort_values('length').head()

ndf = ndf[ndf.seqid.isin(chrs)]
chr_gene_counts = ndf.groupby('seqid').count().ix[:, 0].sort_values().ix[::-1]
chr_gene_counts

df[(df.type == 'gene') & (df.seqid == 'MT')]

gdf = gdf[gdf.seqid.isin(chrs)]
gdf.drop(['start', 'end', 'score', 'strand', 'phase' ,'attributes'], axis=1, inplace=True)
gdf.sort_values('length').ix[::-1]

cdf = chr_gene_counts.to_frame(name='gene_count').reset_index()
cdf.head(2)

merged = gdf.merge(cdf, on='seqid')
merged

merged[['length', 'gene_count']].corr()

ax = merged[['length', 'gene_count']].sort_values('length').plot(x='length', y='gene_count', style='o-')
# add some margin to both ends of x axis
xlim = ax.get_xlim()
margin = xlim[0] * 0.1
ax.set_xlim([xlim[0] - margin, xlim[1] + margin])
# Label each point on the graph
for (s, x, y) in merged[['seqid', 'length', 'gene_count']].sort_values('length').values:
    ax.text(x, y - 100, str(s))
