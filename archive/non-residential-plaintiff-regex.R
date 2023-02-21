# Regex pattern for identifying non-residential plaintiff names
# Authors: Jacob Goldstein-Greenwood
# Last revised: 2021-12-16

non_res_pla_pattern <- paste0('(?i)', paste0(c(# Indicative general keywords
                                             '(\\bap(ar)?t(ment)?(s)?(\\.)?\\b)',
                                             '(\\bass(et|ociate)s\\b)',
                                             '(\\bbank\\b)',
                                             '(\\bcapital\\b)',
                                             '(\\bco(mpany)?(\\.)?\\b)',
                                             '(\\bcorp(oration|\\.)?\\b)',
                                             '(\\bdevelop(ers?|ments?)\\b)',
                                             '(\\bestate\\b)',
                                             '(\\bfoundation\\b)',
                                             '(\\bgroup\\b)',
                                             '(\\bho(ld|us)ings?\\b)',
                                             '(\\bhome\\b)',
                                             '(\\binvestments?\\b)',
                                             '(\\bleasing\\b)',
                                             '(\\bloan\\b)',
                                             '(\\bl(imi)?t(e)?d(\\.)?\\b)',
                                             '(\\bmanagement\\b)',
                                             '(\\bmobile\\b)',
                                             '(\\bpartners(hips?)?\\b)',
                                             '(\\bpropert(ies|y)\\b)',
                                             '(\\brealty\\b)',
                                             '(\\brentals?\\b)',
                                             '(\\bs(ervice|olution)s?\\b)',
                                             '(\\bsolutions\\b)',
                                             '(\\btenants?\\b)',
                                             '(\\btrust\\b)',
                                             '(\\bunits?\\b)',
                                             '(\\bventures?\\b)',
                                             '(\\bvillage\\b)',
                                             # Indicative abbreviations and characters
                                             '(\\b(p)?l(l?c|td|l{0,2}?p)\\.?\\b)',
                                             '(\\binc\\.?\\b)',
                                             '(\\d+)',
                                             '(\\&)',
                                             # Indicative prepositions and articles
                                             '(\\bof\\b)',
                                             '(\\bon\\b)',
                                             '(\\bat\\b)',
                                             '(\\bthe\\b)',
                                             # Charlottesville-/Albemarle-specific patterns; adding during defuzz test phase
                                             '(\\bcommonwealth\\b)',
                                             '(\\bcrha\\b)',
                                             '(\\bhrec\\.?\\b)',
                                             '(\\bcharlottesville\\b)',
                                             '(\\balbemarle\\b)'),
                                            collapse = '|')
                             )