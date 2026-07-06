package scalafim.fmri.design.event

import scalafim.fmri.hrf.Hrf
import scalafim.fmri.hrf.linalg.Mat

enum EventTermRole:
  case Task, Trialwise, Covariate

enum EventTermColumnRole:
  case Task, Trial, TrialAggregate, Covariate

object EventTermColumnRole:
  def defaultFor(termRole: EventTermRole): EventTermColumnRole =
    termRole match
      case EventTermRole.Task      => EventTermColumnRole.Task
      case EventTermRole.Trialwise => EventTermColumnRole.Trial
      case EventTermRole.Covariate => EventTermColumnRole.Covariate

trait EventModelTerm:
  def data: Mat
  def columnNames: Vector[String]
  def columnRoles: Vector[EventTermColumnRole]
  def keyHint: Option[String]
  def hrfOpt: Option[Hrf]
  def role: EventTermRole

  final def isCovariate: Boolean = role == EventTermRole.Covariate || hrfOpt.isEmpty
  final def isTrialwise: Boolean = role == EventTermRole.Trialwise

  final def resolvedColumnRoles: Vector[EventTermColumnRole] =
    if columnRoles.isEmpty then Vector.fill(data.cols)(EventTermColumnRole.defaultFor(role))
    else
      require(
        columnRoles.length == data.cols,
        s"columnRoles has ${columnRoles.length} entries for ${data.cols} data columns"
      )
      columnRoles

  final def requireColumnMetadata(): Unit =
    require(
      columnNames.length == data.cols,
      s"columnNames has ${columnNames.length} entries for ${data.cols} data columns"
    )
    if columnRoles.nonEmpty then
      require(
        columnRoles.length == data.cols,
        s"columnRoles has ${columnRoles.length} entries for ${data.cols} data columns"
      )
