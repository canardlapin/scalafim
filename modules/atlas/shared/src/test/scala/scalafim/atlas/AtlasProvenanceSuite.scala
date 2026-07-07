package scalafim.atlas

class AtlasProvenanceSuite extends munit.FunSuite:

  private val regions =
    RegionIndex(
      Vector(
        Region(RegionId(1), "A", hemisphere = Some(Hemisphere.Left)),
        Region(RegionId(3), "C", hemisphere = Some(Hemisphere.Right))
      )
    )

  test("AtlasRef converts to typed provenance with source artifacts, support, labels, and citations"):
    val ref =
      AtlasRef.volume(
        family = "toy",
        model = "ToyAtlas",
        templateSpace = SpaceId.MNI152NLin6Asym,
        coordSpace = SpaceId.MNI152,
        resolution = Some("2mm"),
        confidence = Confidence.High,
        artifacts = Vector(
          AtlasArtifact(
            role = ArtifactRole.ParcellationVolume,
            sourceName = "toy-source",
            sourceRef = "toy.nii.gz",
            sourceUrl = Some("https://example.org/toy.nii.gz"),
            citationDoi = Some("10.0000/toy"),
            license = Some("CC-BY-4.0"),
            sha256 = Some("abc123")
          ),
          AtlasArtifact(
            role = ArtifactRole.LabelTable,
            sourceName = "toy-source",
            sourceRef = "toy.tsv",
            license = Some("CC-BY-4.0"),
            sha256 = Some("def456")
          )
        ),
        history = Vector(
          AtlasHistoryStep(
            action = "load",
            fromTemplateSpace = SpaceId.MNI152NLin6Asym,
            toTemplateSpace = SpaceId.MNI152NLin6Asym,
            fromCoordSpace = SpaceId.MNI152,
            toCoordSpace = SpaceId.MNI152,
            status = TransformStatus.Available,
            confidence = Confidence.High,
            details = "Loaded toy atlas."
          )
        )
      )

    val provenance = ref.toProvenance(regions)

    assertEquals(provenance.identity.family, "toy")
    assertEquals(provenance.identity.variant, Some("2mm"))
    assertEquals(provenance.sourceArtifacts.map(_.role), Vector(ArtifactRole.ParcellationVolume, ArtifactRole.LabelTable))
    assertEquals(provenance.labels.regionIds, Vector(RegionId(1), RegionId(3)))
    assertEquals(provenance.labels.labelTableArtifactId, Some("label_table_toy_tsv"))
    assertEquals(provenance.citations, Vector(Citation.doi("10.0000/toy")))
    assertEquals(provenance.validate(strict = true), Vector.empty)

    provenance.support match
      case SpatialSupport.Volume(template, coord, Some(size), _) =>
        assertEquals(template, SpaceId.MNI152NLin6Asym)
        assertEquals(coord, SpaceId.MNI152)
        assertEquals(size, VoxelSize.isotropic(2.0))
      case other => fail(s"expected volume support, got $other")

    assert(provenance.derivation.exists(_.isInstanceOf[DerivationStep.Loaded]), clue = provenance.derivation.toString)
    assert(provenance.derivation.exists(_.isInstanceOf[DerivationStep.LegacyHistory]), clue = provenance.derivation.toString)

  test("standard atlas provenance infers release metadata and readable summaries"):
    val schaefer = Schaefer2018.default.atlasRef().toProvenance(regions)
    val glasser = GlasserHcpMmp1Surface.default.atlasRef().toProvenance(regions)

    assertEquals(schaefer.identity.release.flatMap(_.year), Some(2018))
    assertEquals(glasser.identity.release.flatMap(_.version), Some("1.0"))
    assert(schaefer.summaryLines.exists(_.contains("commit=unknown")), clue = schaefer.summary)
    assert(glasser.sourceSummary.exists(_.contains("license=unspecified")), clue = glasser.sourceSummary.mkString("\n"))
    assert(glasser.summaryLines.exists(_.startsWith("support: surface")), clue = glasser.summary)

  test("descriptor-only provenance is explicit and validation reports audit gaps"):
    val ref =
      AtlasRef.surface(
        family = "toy",
        model = "DescriptorOnly",
        templateSpace = SpaceId.Unknown,
        coordSpace = SpaceId.Unknown,
        density = Some("unknown"),
        confidence = Confidence.Uncertain,
        lineage = Some("Declared only for tests.")
      )

    val provenance = ref.toProvenance(regions)
    val issues = provenance.validate(strict = true)

    assertEquals(provenance.sourceArtifacts.map(_.role), Vector(ArtifactRole.Descriptor))
    assert(issues.contains(ProvenanceIssue.NoLoadedArtifact), clue = issues.toString)
    assert(issues.contains(ProvenanceIssue.UncertainConfidence), clue = issues.toString)
    assert(issues.exists {
      case ProvenanceIssue.UncertainSpace(SpaceId.Unknown) => true
      case _ => false
    }, clue = issues.toString)
    assert(issues.exists {
      case ProvenanceIssue.MissingDigest(ArtifactRole.Descriptor) => true
      case _ => false
    }, clue = issues.toString)
    assert(!issues.exists(_.isInstanceOf[ProvenanceIssue.MissingLicense]), clue = issues.toString)

  test("loaded provenance records label filtering decisions"):
    val ref =
      AtlasRef.volume(
        family = "toy",
        model = "Filtered",
        templateSpace = SpaceId.MNI152,
        coordSpace = SpaceId.MNI152,
        confidence = Confidence.Exact,
        artifacts = Vector(
          AtlasArtifact(
            role = ArtifactRole.LabelTable,
            sourceName = "toy",
            sourceRef = "toy.tsv"
          )
        )
      )

    val provenance = AtlasProvenance.loaded(ref, regions, Vector(RegionId(1), RegionId(2), RegionId(3)))

    assert(provenance.derivation.exists {
      case DerivationStep.FilteredLabels(kept, dropped) =>
        kept == Vector(RegionId(1), RegionId(3)) && dropped == Vector(RegionId(2))
      case _ => false
    }, clue = provenance.derivation.toString)

  test("license and voxel-size helpers round-trip legacy metadata"):
    assertEquals(LicenseInfo.fromLegacy(Some("CC-BY-4.0")), LicenseInfo.Known("CC-BY-4.0"))
    assertEquals(LicenseInfo.fromLegacy(Some("Restricted: HCP data use terms")), LicenseInfo.Restricted("HCP data use terms"))
    assertEquals(LicenseInfo.fromLegacy(Some("Unspecified: consult upstream")), LicenseInfo.Unspecified("consult upstream"))
    assertEquals(LicenseInfo.fromLegacy(None), LicenseInfo.Missing)
    assertEquals(LicenseInfo.Restricted("DUA", Some("https://example.org/terms")).legacyString, Some("Restricted: DUA (https://example.org/terms)"))
    assertEquals(LicenseInfo.Unspecified("consult upstream").summary, "unspecified: consult upstream")

    assertEquals(VoxelSize.parse("2mm").map(_.label), Some("2mm"))
    assertEquals(VoxelSize.parse("2x3x4mm").map(_.label), Some("2x3x4mm"))
    assertEquals(VoxelSize.parse("1.5mm").map(_.label), Some("1.5mm"))
    assertEquals(VoxelSize.parse("2x3mm"), None)
    assertEquals(VoxelSize.parse("not-a-size"), None)

  test("source artifacts preserve restricted licenses and SHA digests in legacy export"):
    val source =
      SourceArtifact(
        id = "volume",
        role = ArtifactRole.ParcellationVolume,
        sourceName = "toy-source",
        sourceRef = "toy.nii.gz",
        sourceUri = Some("https://example.org/toy.nii.gz"),
        citationDoi = Some("10.0000/toy"),
        licenseInfo = LicenseInfo.Restricted("research use", Some("https://example.org/terms")),
        digest = Some(Digest.sha256("abc123")),
        notes = Some("loaded from local test fixture")
      )

    val artifact = source.toAtlasArtifact
    assertEquals(artifact.role, ArtifactRole.ParcellationVolume)
    assertEquals(artifact.sourceUrl, Some("https://example.org/toy.nii.gz"))
    assertEquals(artifact.citationDoi, Some("10.0000/toy"))
    assertEquals(artifact.license, Some("Restricted: research use (https://example.org/terms)"))
    assertEquals(artifact.sha256, Some("abc123"))
    assertEquals(artifact.notes, Some("loaded from local test fixture"))

  test("strict provenance validation flags missing source accounting"):
    val ref =
      AtlasRef.volume(
        family = "toy",
        model = "MissingSourceAccounting",
        templateSpace = SpaceId.MNI152,
        coordSpace = SpaceId.MNI152,
        confidence = Confidence.High,
        artifacts = Vector(
          AtlasArtifact(
            role = ArtifactRole.LabelTable,
            sourceName = "toy-labels",
            sourceRef = "toy.tsv"
          )
        )
      )

    val provenance = ref.toProvenance(regions)
    val issues = provenance.validate(strict = true)

    assert(issues.contains(ProvenanceIssue.MissingDigest(ArtifactRole.LabelTable)), clue = issues.toString)
    assert(issues.contains(ProvenanceIssue.MissingLicense("toy-labels")), clue = issues.toString)

    val emptySources = intercept[IllegalArgumentException]:
      provenance.withSourceArtifacts(Vector.empty)
    assert(emptySources.getMessage.contains("NonEmptyVector requires at least one value"), clue = emptySources.getMessage)
