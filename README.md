# Introduction to Haskell

## Structure of this course / Expectations

You will need to spend a significant amount of time (six to eight
hours) per day on this course!

You will be expected to

- follow pre-recorded videos on your own, introducing various
  Haskell topics, according to a timetable (see below),

- try to answer self-test exercises associated with the videos,

- self-report your progress every day on the Slack channel,
  and/or ask questions you might have regarding the videos and
  self-test questions,

- work on course assignments on your own, writing Haskell code,
  and submit them via Gitlab; there are multiple blocks of
  assignments, with deadlines for each of them, and you will receive
  individual feedback on these assignments,

- attend scheduled video calls where we discuss the course topics,
  you might present solutions to some of the assignments, and
  we can answer further questions,

- write a final exam (remotely), where you again
  will have to write and submit some Haskell code, which will
  subsequently be reviewed and graded,

Any feedback on this format or other aspects
of the course is welcome at all times!

## Evaluation

Evaluation will be based on the following criteria:

- Regular self-reporting on the slack channel every day for the
  duration of the course, listing progress and potential questions.

- Regular attendance of the calls.

- Interactions via Slack and during the calls. The more questions
  you ask, the better.

- Regular submission of the assignments in time. You have to do
  the assignments on your own!

- Being able to explain your solutions to the assignments and reflect
  on them in the calls and/or on the Slack channel.

- Performance in the final exam.

## Timetable for videos and assignments

Daily Q&A sessions will focus on specific parts of the videos.
The topic of each call will be generally announced on Slack
before the meetings, but unless there is a specific need
it will be:

| Weekday   | Topic    |
|-----------|----------|
| Monday    | Part 1&2 |
| Tuesday   | Part 3&4 |
| Wednesday | Part 5&6 |
| Thursday  | Part 1&2 |
| Friday    | Part 3&4 |

You should submit assignments at your own pace, but bear in
mind that the course is designed to take two to three weeks
of your time. This means that you submit assignment every second
day. For you reference, here is a typical/expected
number of days since you start the course to submit assignments:

| Assignment | Days since start date |
|------------|-----------------------|
| A          | 2 days                |
| B          | 4 days                |
| C          | 6 days                |
| D          | 8 days                |
| E          | 10 days               |
| F          | 13 days               |
| X          | 15 days               |

Calls will usually be about 60-90 minutes, depending on the amount of
questions and discussions. Please try to join the calls on time.

The exam will be two hours.

## Videos

The slides and assignments will be distributed via this repository. The videos
themselves are all visible via [this link](https://courses.pages.well-typed.com/haskell-intro/).
You need to be logged into the Well-Typed Gitlab to access it.

The site contains an overview of the course structure, and all the individual numbered
videos. It also shows at which point in the course you are supposed to work on the
assignments.

Every video has a set of "Self Test" questions. These, you should be able to answer on
your own after having watched a video. You don't need to submit your answers, but if you
are uncertain or have difficulties with these tasks, you should ask for help.

Starting from Part 2, some videos are marked "Optional". These are not really
required for the course, but may help understanding.

## Repository structure

The following directories will be used throughout the
course:

- The `assignments` directory contains the reference version of
  the various assignment sets.
- The `demo` directory contains additional code we may develop
  during calls.

## Slack

For questions and announcements surrounding the course, we will
use the `#haskell-training` channel on the Juspay slack.

Use the channel rather than direct messages for questions. Don't be
afraid to ask questions. Asking questions is an important part of
learning. We welcome questions!

## Software requirements

Please look [here][Setup] for detailed instructions.

## Submitting assignments

Submit assignments by creating a branch in the repository and creating a merge
request. Each time you create a new branch for your solution start from the
*main* branch. Please name your branch with your `your.name-assignment`, e.g.
`your.name-a` for assignment A, `your.name-b` for assignment B, etc. This
ensures that we don't have branch name conflicts.

Before making merge request please make sure that your branch contains all most
recent changes from the main branch, either by rebasing (if you did not yet
pushed your branch) or merging (otherwise).

In your branch you should modify
only one file: corresponding to the assignment you are currently
submitting. E.g., when submitting assignment C, only `assignments/c/C.hs`
file should be committed.

When you open a merge request a CI pipeline will run on your
branch. It consists of three stages: prepare, check and test.
You can see their status on your merge request page (see *Pipielines* tab).
If all stages pass (are green), then you are all good
(though you will still be graded and received individual feedback on style).

If the first stage *prepare* fail let us know on Slack
channel -- that is something that should not happen.

If the second stage *check* fails you likely did not followed
instructions on assignment submission: either main branch contains
newer changes or you submitted more than one assignment. Please 
check details of your failed pipeline, check the *Tests* tab to see 
which tests failed. Consult test details for further help.

If you still don't know why your *check* stage failed, 
let us know on Slack.

Finally, if the third stage *test* fails then likely part of
your solution is wrong. Again, you can check the *Tests* tab 
of detailed view of the pipeline to see which tests failed and why.

[Setup]: https://courses.pages.well-typed.com/haskell-intro/setup.html
