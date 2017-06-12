import pandas as pd

import os
os.getcwd()

os.chdir("U:\\Github_blog")

col_names = ['seqid', 'source', 'type', 'start', 'end', 'score', 'strand', 'phase', 'attributes']

df = pd.read_csv('Homo_sapiens.GRCh38.85.gff3.gz', compression='gzip',
                         sep='\t', comment='#', low_memory=False,
                         header=None, names=col_names)
                         
df.head()
df.info()

gdf = df[df.source == 'GRCh38']
gdf.shape
gdf.sample(10)

gdf = gdf.copy()
gdf['length'] = gdf.end - gdf.start + 1
gdf.head()

gdf.length.sum()

chrs = [str(_) for _ in range(1, 23)] + ['X', 'Y', 'MT']
gdf[-gdf.seqid.isin(chrs)].length.sum() / gdf.length.sum()

gdf[(gdf['type'] == 'supercontig')].length.sum() / gdf.length.sum()

edf = df[df.source.isin(['ensembl', 'havana', 'ensembl_havana'])]
edf.sample(10)

edf.type.value_counts()

ndf = edf[edf.type == 'gene']
ndf = ndf.copy()
ndf.sample(10).attributes.values

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

count_df = ndf.groupby('gene_name').count().ix[:, 0].sort_values().ix[::-1]
count_df.head(10)

ndf[ndf.gene_name == 'SCARNA20']

ndf['length'] = ndf.end - ndf.start + 1
ndf.length.describe()

install pyplot
install cycler
install sys
install lib
install matplotlib.rcsetup

from distutils.core import setup  
import py2exe
import sys
import lib
from lib import mod_a 

import cycler
import matplotlib as plt

ndf.length.plot(kind='hist', bins=50, logy=True)
plt.show()