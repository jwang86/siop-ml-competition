# SIOP Machine Learning Competition (2020)

This year’s competition is essentially an multi-objective optimization problem within selection.

- Competition details can be found **[here](https://drive.google.com/file/d/1MW9Fli3ykCkP8dPuCll78rvyaGyxyxAC/view)**.

- Description of the data can be found **[here](https://drive.google.com/file/d/1_Ve4jRoYsj5GB62_9BXdJGjstl_2KsZ5/view)**.

- Any questions? **[FAQ](https://docs.google.com/document/d/1gxWAl5jMtZXabcOPd2ivTT-C3KAXA_E3BLHcF80dtXs/edit)**

This is open to all!


## BRAINSTORM
* What to do with SJ and senario items? Does processing time matter?
* Do we use all biodata? If not, how do we filter it?
* What are we trying to optimize?
* What predictors should we use?
* What do we do about missing data?
* How do we make use of the train and dev sets?


## PROGRESS CHECK

### Readings
- [ ] Read MOO & GPareto articles **[here](https://drive.google.com/drive/u/0/folders/1sm5aJdyIsk_2Rp4CdAtab7duamQWFXCg)**
- [ ] Read task and data description **[here](https://drive.google.com/file/d/1_Ve4jRoYsj5GB62_9BXdJGjstl_2KsZ5/view)**

### Data Processing
* Look at item descriptives
- [x] Item descriptives for all variables
- [x] Look at correlation among criteria & fairness
- [x] Run correlation/alpha to identify reverse coded items/remove bad items
- [ ] How predictors relate to criteria and fairness
* Create composite variables
- [ ] Personality subscales

### MOO
- [ ] Create pareto space
- [ ] Get results for pareto space
- [ ] Sort results
- [ ] Run GPareto


## NOTES
Items that needs to be reverse coded:
* pscale01: q2, q3
* pscale02: q2
* pscale03: q1
* pscale04: q1
* pscale05: na
* pscale06: q3, q6
* pscale07: q2
* pscale08: q2, q4
* pscale09: q2
* pscale10: q3, q4
* pscale11: q2, q3
* pscale12: q3, q4
* pscale13: q3, q4
