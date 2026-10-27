package scalafim.fmri.group

import neuroim4s.spaces.SubjectId

class SharedSubjectSuite extends munit.FunSuite:
  test("group axes consume shared subject identities in declared order"):
    val subjects = Vector(" study/sub-02 ", "study/sub-01").map(value => SubjectId(value).toOption.get)
    val axis = SubjectAxis.from(subjects).toOption.get
    assertEquals(axis.subjects.map(_.value), Vector("study/sub-02", "study/sub-01"))

  test("normalization collisions retain the group duplicate-subject error"):
    val subjects = Vector(" sub-01 ", "sub-01").map(value => SubjectId(value).toOption.get)
    assertEquals(SubjectAxis.from(subjects), Left(GroupError.DuplicateSubjects(Vector("sub-01"))))
