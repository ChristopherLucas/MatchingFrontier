Replication materials for:

Nielsen, Richard, Michael Findley, Zachary Davis, Tara Candland, and Daniel Nielsen. "Foreign Aid Shocks as a Cause of Violent Armed Conflict," Forthcoming at the American Journal of Political Science.

Rich Nielsen
nielsen.rich@gmail.com
29 December, 2010

There are "big" and "small" versions of the replication archive.  The big archive contains (almost) all of the files and code to reproduce the data and analysis.  The small archive has all of the data and code necessary to reproduce the analysis, but not the data.

The archive contains the following files and folders:

readme.txt                     - this file
cwdata                         - the dataset
CW analysis-ajps-part1.do      - build the data
CW analysis-ajps-part2.do      - main analysis
CW analysis-ajps-part3.do      - analysis with alternative sources of aid data
data                           - a file containing all of the raw, unmerged data
                                 (big archive only).  A full list of the data files 
                                 is below.

Workflow:

1. unzip the replication package to your preferred location.  Leave the "data" folder inside of the main replication package folder so that the code can find it.

2. Open both Stata and R and set the working directory in both to the location of the replication package: ex "C:/My documents/stuff/archive".  

3. Replicate.

Option 1: To replicate the entire analysis, including creating the data from scratch, start with "CW analysis-ajps-part1.do" and then move to "CW analysis-ajps-part2.do" and "CW analysis-ajps-part3.do" (big archive only).

Option 2: To replicate the analysis without building the data from scratch, use "CW analysis-ajps-part2.do" and "CW analysis-ajps-part3.do".

With the small archive, only option 2 is available.  However, the file "CW analysis-ajps-part1.do" may still be helpful for figuring out how the variables were constructed.


Notes:

The analysis was done primarily in Stata 9 (with some in version 10 and 11) with the "relogit", "clarify", and "btscs" packages installed.  Some analysis was done in R (several versions, with the latest being 2.10.1). Most of the analysis was run on an Asus netbook running Windows XP.  Some of it was done on Linux servers housed in IQSS at Harvard University.

The code files in this archive are only slightly different from the actual files we used.  I've tried to add comments wherever we did something that was really confusing or ambiguous, but we haven't touched it up much other than that.  So there is plenty of random stuff that may not seem relevant -- false starts, mistakes, inefficient code, odd naming conventions, etc.

Note that the scripts are in Stata's .do format, but contain both Stata and R code.  Comments will tell you when you need to copy and past some chunk of code into R.  You can also tell R code from stata code because stata comments use "*" while R comments use "#".  As long as your working directory in R is set to the main archive folder, you should be able to copy and paste without making any changes.  Why use both Stata and R?  Because it was easier that way.

The scripts will create new folders within the archive folder: 

The first is "output" which holds the key files produced by the analysis.  After running the code, look there for tables and figures that appear in the article/appendix.  Note that in many cases, we touched these up manually, so the formatting may be different. 

The second is "crap" which holds temporary files that are produced along the way.  After you have run the analysis, you can delete the "crap" file and all of it's contents.   



The folder "data" contains the following files:

  expgdp_v5.0.asc                                           - Gleditsch GDP data
  Selected WDI vars.dta                                     - World Bank data
  Standardize Country Codes.demaid.do                       - standardizes country codes
  Standardize Country Names.demaid.do                       - standardizes country names
  prio.txt                                                  - PRIO conflict data
  Standardize Country Codes.master do file.08.28.07.do      - standardizes country codes
  Standardize Country Names.master do file.RHR.08.28.07.do  - standardizes country names
  p4v2006.csv                                               - Polity IV data
  PITFp4data.dta                                            - PITF data
  New Standardize Country Names.do                          - standardizes country names
  MEPV04ex.csv                                              - PITF data
  MAR.17feb2010.csv                                         - Minorities at risk data
  water and health.csv                                      - More World Bank indicators
  CIRI 2007.csv                                             - CIRI human rights data
  PTS.dta                                                   - Political Terror Scale
  COW convert.do                                            - Converts COW codes
  CNTSDATA.csv                                              - Banks riot data
  ISA version.dta                                           - An old version of the dataset
                                                              that has some variables we
                                                              can't remember how to build 
                                                              from scratch.
  Ross Oil & Gas 1932-2006 public.dta                       - Oil data (Ross)
  data1990.csv                                              - OECD CRS aid data
  data1991.csv                                              - OECD CRS aid data
  data1992.csv                                              - OECD CRS aid data
  data73-79.csv                                             - OECD CRS aid data
  data80-89.csv                                             - OECD CRS aid data
  data1993.csv                                              - OECD CRS aid data
  data1994.csv                                              - OECD CRS aid data
  data1995.csv                                              - OECD CRS aid data
  data1996.csv                                              - OECD CRS aid data
  data1997.csv                                              - OECD CRS aid data
  data1998.csv                                              - OECD CRS aid data
  data1999.csv                                              - OECD CRS aid data
  data2000.csv                                              - OECD CRS aid data
  data2001.csv                                              - OECD CRS aid data
  data2002.csv                                              - OECD CRS aid data
  data2003.csv                                              - OECD CRS aid data
  data2004_1.csv                                            - OECD CRS aid data
  data2004_2.csv                                            - OECD CRS aid data
  data2005_1.csv                                            - OECD CRS aid data
  data2005_2.csv                                            - OECD CRS aid data
  data2006_1.csv                                            - OECD CRS aid data
  data2006_2.csv                                            - OECD CRS aid data
  plaid for aidshocks.dta                                   - Processed PLAID/AidData data
  plaid disb for aidshocks.dta                              - Processed Disbursement data
  GBK1.csv                                                  - USAID greenbook
  GBK2.csv                                                  - USAID greenbook
  GBK3.csv                                                  - USAID greenbook
  GBK4.csv                                                  - USAID greenbook
  GBK5.csv                                                  - USAID greenbook
  GBK6.csv                                                  - USAID greenbook
  greenbook.csv                                             - Processed USAID greenbook
  SC rotating members.dta                                   - Data on UNSC membership
  make UN SC member data.smcl                               - How to make the UNSC data
  affinity_03102006.dta                                     - UN voting affinity

