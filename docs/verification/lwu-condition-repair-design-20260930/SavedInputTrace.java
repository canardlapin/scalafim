import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import scalafim.fmri.fit.profile.*;
import scalafim.fmri.hrf.family.ParametricHrfFamily;
import scalafim.fmri.laws.profile.ConditionMilestoneSuite;

/** Read-only delegate: every objective result is returned unchanged to the frozen decoder. */
class SavedInputTrace {
  static String array(double[] x) {
    StringBuilder b=new StringBuilder("[");
    for(int i=0;i<x.length;i++){if(i>0)b.append(',');b.append('"').append(Double.toHexString(x[i])).append('"');}
    return b.append(']').toString();
  }
  static String vector(scala.collection.immutable.Vector<Object> x) {
    double[] out=new double[x.length()];
    for(int i=0;i<out.length;i++)out[i]=((Number)x.apply(i)).doubleValue();
    return array(out);
  }
  static class Traced implements ShapeObjective {
    final CompactConditionObjective original;
    final CompactConditionJets designs;
    String id; int ordinal;
    Traced(CompactConditionObjective o) throws Exception {original=o;var field=CompactConditionObjective.class.getDeclaredField("jets");field.setAccessible(true);designs=(CompactConditionJets)field.get(o);}
    public NodeGrid grid(){return original.grid();}
    public int amplitudeCount(){return original.amplitudeCount();}
    public double scoreNode(int node){return original.scoreNode(node);}
    void emit(String event,double[] x,ProfileJetBuffer out,boolean ok){
      System.out.println("{\"kind\":\"jvm-lwu-trace\",\"case\":\""+id+"\",\"ordinal\":"+(ordinal++)+",\"event\":\""+event+"\",\"ok\":"+ok+",\"coordinatesHex\":"+array(x)+",\"energyHex\":\""+Double.toHexString(out.energy())+"\",\"gradientHex\":"+array(out.gradient())+",\"hessianHex\":"+array(out.hessian())+",\"amplitudesHex\":"+array(out.amplitudes())+",\"designHex\":"+array(designs.valueDesign())+"}");
    }
    public boolean jetAtNode(int node,ProfileJetBuffer out){
      boolean ok=original.jetAtNode(node,out);double[] x=new double[3];grid().coordinatesInto(node,x);emit("nodeJet",x,out,ok);return ok;
    }
    public boolean jetAt(double[] x,ProfileJetBuffer out){boolean ok=original.jetAt(x,out);emit("jet",x,out,ok);return ok;}
    public double energyAt(double[] x,ProfileJetBuffer out){double e=original.energyAt(x,out);emit("energy",x,out,Double.isFinite(e));return e;}
  }
  @SuppressWarnings("unchecked")
  public static void main(String[] args) throws Exception {
    if(args.length!=1)throw new IllegalArgumentException("saved input directory required");
    ConditionMilestoneSuite fixture=new ConditionMilestoneSuite();
    Method prepare=ConditionMilestoneSuite.class.getDeclaredMethod("lwu");prepare.setAccessible(true);
    scala.Tuple2<?,?> tuple=(scala.Tuple2<?,?>)prepare.invoke(fixture);
    CompactConditionPreparation prep=(CompactConditionPreparation)tuple._2();
    Method nodes=ConditionMilestoneSuite.class.getDeclaredMethod("nodesFor",ParametricHrfFamily.class);nodes.setAccessible(true);
    Method policy=ConditionMilestoneSuite.class.getDeclaredMethod("budgetFor",ParametricHrfFamily.class);policy.setAccessible(true);
    NodeGrid grid=new NodeGrid(prep.family().chart(),(scala.collection.immutable.Vector<Object>)nodes.invoke(fixture,prep.family()));
    DecodeBudget budget=(DecodeBudget)policy.invoke(fixture,prep.family());
    Traced traced=new Traced(new CompactConditionObjective(prep,grid));
    ShapeDecoder decoder=new ShapeDecoder(traced,budget,scala.Option$.MODULE$.empty(),1.0);
    System.out.println("{\"kind\":\"trace-runtime\",\"java\":\""+System.getProperty("java.version")+"\",\"prepRank\":"+prep.rank()+"}");
    var basis=prep.basis();double[] phi=new double[basis.rank()*basis.fineCount()];
    for(int j=0;j<basis.rank();j++)for(int i=0;i<basis.fineCount();i++)phi[j*basis.fineCount()+i]=basis.value(j,i);
    System.out.println("{\"kind\":\"frozen-compact-operators\",\"rank\":"+prep.rank()+",\"conditions\":"+prep.conditions()+",\"basisRank\":"+basis.rank()+",\"lagsHex\":"+array(basis.lags())+",\"phiHex\":"+array(phi)+",\"rHatHex\":"+array(prep.rHat())+"}");
    List<Path> paths;
    try(var stream=Files.list(Path.of(args[0]))){paths=stream.filter(p->p.getFileName().toString().endsWith(".hex")).sorted().toList();}
    if(paths.size()!=9)throw new IllegalArgumentException("exact nine saved-input controls required");
    for(Path path:paths){
      List<String> lines=Files.readAllLines(path);if(lines.size()!=600)throw new IllegalArgumentException("600 rows required");
      double[] y=new double[600];for(int i=0;i<600;i++)y[i]=Double.valueOf(lines.get(i));
      double[] z=new double[prep.rank()];double[] qy=new double[prep.nuisanceRank()];
      double e=prep.project(y,0,z,qy);traced.original.pointAt(z,e);
      traced.id=path.getFileName().toString();traced.ordinal=0;
      System.out.println("{\"kind\":\"trace-projection\",\"case\":\""+traced.id+"\",\"energyHex\":\""+Double.toHexString(e)+"\",\"zHex\":"+array(z)+"}");
      DecoderCounters counters=new DecoderCounters();ShapeDecodeResult r=decoder.decode(counters);
      System.out.println("{\"kind\":\"trace-result\",\"case\":\""+traced.id+"\",\"status\":\""+r.status()+"\",\"coordinatesHex\":"+vector(r.coordinates())+",\"energyHex\":\""+Double.toHexString(r.energy())+"\",\"work\":["+counters.nodeScores()+","+counters.jets()+","+counters.exactEvaluations()+","+counters.candidateAttempts()+","+counters.terminalVerifications()+","+counters.newtonSteps()+","+counters.fallbacks()+"]}");
    }
  }
}
