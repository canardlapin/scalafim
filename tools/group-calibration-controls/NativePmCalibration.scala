package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

/** Reads explicit independent-study columns; executes the ordinary provider API. */
object NativePmCalibration:
  def main(args: Array[String]): Unit =
    require(args.length==2,"input and output paths required")
    val input=new DataInputStream(new BufferedInputStream(new FileInputStream(args(0))))
    val (n,p,samples,x,y,v)=try
      require(input.readInt()==0x47504331,"input format GPC1")
      val n=input.readInt();val p=input.readInt();val samples=input.readInt()
      require(n>p && p==3 && samples>0)
      val x=Matrix.tabulate(n,p)((_,_)=>input.readDouble())
      val y=Matrix.tabulate(n,samples)((_,_)=>input.readDouble())
      val v=Matrix.tabulate(n,samples)((_,_)=>input.readDouble())
      require(input.read()== -1,"no trailing input bytes")
      (n,p,samples,x,y,v)
    finally input.close()
    def get[A](v: Either[GroupError,A]): A=v.fold(e=>throw new IllegalArgumentException(e.message),identity)
    val data=get(GroupData.withVariances(Vector.tabulate(n)(i=>SubjectId(s"subject-$i")),GroupSpace.SampleAxis(samples),"response",y,v))
    val design=get(GroupDesign.fromMatrix(x,Vector("intercept","group","covariate")))
    val started=System.nanoTime()
    val result=get(GroupModel.mixedEffects(data,design,TauEstimator.PauleMandel,MetaInference.ModifiedKnappHartung).flatMap(GroupEngine.fit(_)))
    val contrast=get(result.contrast("response",GroupContrast.term("group")))
    val tau=result.fit("response").get.heterogeneity.get.tau2
    val output=new DataOutputStream(new BufferedOutputStream(new FileOutputStream(args(1))))
    try
      output.writeInt(0x47505231);output.writeInt(samples)
      var i=0
      while i<samples do
        output.writeDouble(contrast.estimates(i));output.writeDouble(contrast.standardErrors(i))
        output.writeDouble(contrast.statistics(i));output.writeDouble(contrast.pValues(i));output.writeDouble(tau(i))
        i+=1
    finally output.close()
    println(s"""{"subjects":$n,"terms":$p,"studies":$samples,"failures":${contrast.failures.length},"fitSeconds":${(System.nanoTime()-started)/1e9}}""")
