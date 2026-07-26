package scalafim.bids

class BidsValidationSuite extends munit.FunSuite:
  test("dataset descriptions accumulate independent required and typed field defects"):
    val path = BidsPath("dataset_description.json")
    val json: JsonValue.Obj = JsonValue.Obj(Map(
      "Name" -> JsonValue.Str(""),
      "DatasetType" -> JsonValue.Str("unsupported"),
      "DatasetLinks" -> JsonValue.Obj(Map(
        "empty" -> JsonValue.Str(""),
        "typed" -> JsonValue.Num(1.0)
      ))
    ))

    val issues = BidsMetadataValidation.datasetDescription(path, json)

    assertEquals(
      issues.map(issue => (issue.code, issue.field)),
      Vector(
        BidsIssueCode.InconsistentMetadata -> Some("Name"),
        BidsIssueCode.MissingRequiredField -> Some("BIDSVersion"),
        BidsIssueCode.InconsistentMetadata -> Some("DatasetType"),
        BidsIssueCode.InconsistentMetadata -> Some("DatasetLinks.empty"),
        BidsIssueCode.InvalidSidecar -> Some("DatasetLinks.typed")
      )
    )

  test("resolved sidecar validation accumulates independent metadata defects"):
    val path = BidsPath("sub-01/func/sub-01_task-rest_bold.nii.gz")
    val json: JsonValue.Obj = JsonValue.Obj(Map(
      "TaskName" -> JsonValue.Str(""),
      "RepetitionTime" -> JsonValue.Num(-1.0),
      "VolumeTiming" -> JsonValue.Arr(Vector(JsonValue.Num(0.0), JsonValue.Num(0.0))),
      "SliceTiming" -> JsonValue.Arr(Vector(JsonValue.Num(0.0), JsonValue.Str("bad")))
    ))

    val issues = BidsMetadataValidation.resolvedSidecar(path, json)

    assertEquals(
      issues.map(issue => (issue.code, issue.field)),
      Vector(
        BidsIssueCode.InconsistentMetadata -> Some("TaskName"),
        BidsIssueCode.InconsistentMetadata -> Some("RepetitionTime"),
        BidsIssueCode.InconsistentMetadata -> Some("VolumeTiming"),
        BidsIssueCode.InvalidSidecar -> Some("SliceTiming"),
        BidsIssueCode.InconsistentMetadata -> Some("RepetitionTime")
      )
    )
