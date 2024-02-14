# Regex pattern for identifying management company plaintiffs
# Authors: Jacob Goldstein-Greenwood
# Last revised: 08-06-2021

mgmt_comp_pattern <- paste0('(?i)', paste0(c(# Indicative general keywords
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
                                             '(\\binvestments?\\b)',
                                             '(\\bleasing\\b)',
                                             '(\\bl(imi)?t(e)?d(\\.)?\\b)',
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
                                             # Charlottesville-/Albemarle-specific patterns
                                             '(\\bcommonwealth\\b)',
                                             '(\\bcrha\\b)',
                                             '(\\bhrec\\.?\\b)',
                                             '(\\bcharlottesville\\b)',
                                             '(\\balbemarle\\b)'),
                                            collapse = '|')
                             )