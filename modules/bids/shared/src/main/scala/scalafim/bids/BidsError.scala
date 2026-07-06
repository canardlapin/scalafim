package scalafim.bids

enum BidsError:
  case EmptyPath
  case InvalidPath(path: String, detail: String)
  case InvalidQuery(detail: String)
  case InvalidBidsName(filename: String, detail: String)
  case DuplicateEntity(key: String)
  case InvalidEntityValue(key: String, value: String, detail: String)
  case UnknownEntity(key: String)
  case InvalidBidsUri(uri: String, detail: String)
  case UnknownDatasetLink(name: String, available: Vector[String])
  case InvalidTable(detail: String)
  case InvalidJson(detail: String)
  case MissingParticipants(path: String)
  case Io(path: String, detail: String)
  case UnknownConfoundSet(name: String)
  case NoConfoundColumns(requested: Vector[String], available: Vector[String])
  case InvalidConfoundStrategy(name: String, detail: String)

  def message: String =
    this match
      case EmptyPath =>
        "BIDS path must be non-empty"
      case InvalidPath(path, detail) =>
        s"invalid BIDS path '$path': $detail"
      case InvalidQuery(detail) =>
        s"invalid BIDS query: $detail"
      case InvalidBidsName(filename, detail) =>
        s"invalid BIDS filename '$filename': $detail"
      case DuplicateEntity(key) =>
        s"BIDS entity '$key' appears more than once"
      case InvalidEntityValue(key, value, detail) =>
        s"invalid value '$value' for BIDS entity '$key': $detail"
      case UnknownEntity(key) =>
        s"unknown BIDS entity '$key'"
      case InvalidBidsUri(uri, detail) =>
        s"invalid BIDS URI '$uri': $detail"
      case UnknownDatasetLink(name, available) =>
        s"DatasetLinks does not contain '$name'; available keys: ${available.mkString(", ")}"
      case InvalidTable(detail) =>
        s"invalid BIDS table: $detail"
      case InvalidJson(detail) =>
        s"invalid JSON: $detail"
      case MissingParticipants(path) =>
        s"participants.tsv is missing at '$path'"
      case Io(path, detail) =>
        s"I/O failure at '$path': $detail"
      case UnknownConfoundSet(name) =>
        s"unknown confound set '$name'"
      case NoConfoundColumns(requested, available) =>
        s"none of the requested confounds were found: ${requested.mkString(", ")}; available columns: ${available.mkString(", ")}"
      case InvalidConfoundStrategy(name, detail) =>
        s"invalid confound strategy '$name': $detail"
