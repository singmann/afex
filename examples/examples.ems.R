# 2x2 mixed anova
# A varies between-subjects, B varies within-subjects
ems(r ~ A*B*S, nested="A/S", random="S")

# Clark (1973) example
# random Subjects, random Words, fixed Treatments
ems(r ~ S*W*T, nested="T/W", random="SW")

# EMSs for Clark design if Words are fixed
ems(r ~ S*W*T, nested="T/W", random="S")

