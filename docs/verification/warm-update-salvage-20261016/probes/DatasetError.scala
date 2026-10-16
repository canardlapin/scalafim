package scalafim.dataset
enum DatasetError:
  case StorageFailure(detail: String)
  def message: String = this match
    case StorageFailure(value) => value
