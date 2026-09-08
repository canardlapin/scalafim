package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

private[mvpa] object PartitionIndependenceTestSupport:
  private val SourceKind = ScientificSourceKind.unsafe("test-partition-evidence")

  def declareAllPairs[S <: SemanticSpace](
      source: ScientificSourceIdentity,
      partitions: PartitionAxis[S],
      id: String
  ): Either[PairingDesignError, PartitionIndependenceDeclaration[S, S]] =
    declarePairs(source, partitions, allPairs(partitions), id)

  def declareAllPairsForDesign[S <: SemanticSpace](
      partitions: PartitionAxis[S],
      id: String
  ): Either[PairingDesignError, PartitionIndependenceDeclaration[S, S]] =
    ScientificSourceIdentity(
      SourceKind,
      Vector(ScientificSourceAxis(partitions.name, partitions.axis.identity)),
      Vector("declaration-owner" -> id)
    ).left
      .map(PairingDesignError.InvalidIdentity.apply)
      .flatMap(declareAllPairs(_, partitions, id))

  def declarePairs[S <: SemanticSpace](
      source: ScientificSourceIdentity,
      partitions: PartitionAxis[S],
      pairs: Seq[DeclaredIndependentPair],
      id: String
  ): Either[PairingDesignError, PartitionIndependenceDeclaration[S, S]] =
    for
      evidence <- PartitionEvidenceIdentity(source, partitions)
      declaration <- PartitionIndependenceDeclaration(
        IndependenceDeclarationId.unsafe(id),
        evidence,
        evidence,
        pairs
      )
    yield declaration

  def declareBetween[L <: SemanticSpace, R <: SemanticSpace](
      leftSource: ScientificSourceIdentity,
      left: PartitionAxis[L],
      rightSource: ScientificSourceIdentity,
      right: PartitionAxis[R],
      pairs: Seq[DeclaredIndependentPair],
      id: String
  ): Either[PairingDesignError, PartitionIndependenceDeclaration[L, R]] =
    for
      leftEvidence <- PartitionEvidenceIdentity(leftSource, left)
      rightEvidence <- PartitionEvidenceIdentity(rightSource, right)
      declaration <- PartitionIndependenceDeclaration(
        IndependenceDeclarationId.unsafe(id),
        leftEvidence,
        rightEvidence,
        pairs
      )
    yield declaration

  def allPairs[S <: SemanticSpace](
      partitions: PartitionAxis[S]
  ): Vector[DeclaredIndependentPair] =
    val pairs = Vector.newBuilder[DeclaredIndependentPair]
    var left = 0
    while left < partitions.axis.size do
      var right = left + 1
      while right < partitions.axis.size do
        pairs += DeclaredIndependentPair(
          partitions.axis.keys(left),
          partitions.axis.keys(right)
        )
        right += 1
      left += 1
    pairs.result()
