import scala.sys.process._
import java.io.File

object Judge{

    def compileScala(problem: String, name: String): Boolean=
    {
        val compile = Seq("fsc", name)
        val os = sys.props("os.name").toLowerCase
        val panderToWindows = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ compile
            case _ => compile
        }
        panderToWindows.!
        val compiled = new java.io.File("Solution.class").exists
        return compiled
    }

    def runScala(problem: String, name: String, inputFile: String, outputFile: String): String ={
        val run = Seq("scala", "Solution")
        val inp = Process.cat(new File(inputFile))
        val out = Process.cat(new File(outputFile))
        val os = sys.props("os.name").toLowerCase
        val panderToWindows1 = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ run
            case _ => run
        }
        val result = (inp #> panderToWindows1).!!
        if(out.!! == result) return "OK"
        else return "WA"
    }

    def judgeScala(problem: String, name: String): String={
        if(compileScala(problem, name))
        {
            var i = 1
            var result = "OK"
            while(result == "OK" && i < 4)
            {
                result = runScala(problem, name, raw".\" + problem + raw"\Tests\in" + i + ".txt", raw".\" + problem + raw"\Tests\out" + i + ".txt")
                i += 1
            }
            return result
        }
        else return "CE"
    }

    def compileCpp(problem: String, name: String): Boolean=
    {
        val compile = Seq("g++", "-o", "program", name)
        val os = sys.props("os.name").toLowerCase
        val panderToWindows = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ compile
            case _ => compile
        }
        panderToWindows.!
        val compiled = new java.io.File("program.exe").exists
        return compiled
    }

    def runCpp(problem: String, name: String, inputFile: String, outputFile: String): String ={
        val run = Seq(raw".\program.exe")
        val inp = Process.cat(new File(inputFile))
        val out = Process.cat(new File(outputFile))
        val os = sys.props("os.name").toLowerCase
        val panderToWindows1 = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ run
            case _ => run
        }
        val result = (inp #> panderToWindows1).!!
        if(out.!! == result) return "OK"
        else return "WA"
    }

    def judgeCpp(problem: String, name: String): String={
        if(compileCpp(problem, name))
        {
            var i = 1
            var result = "OK"
            while(result == "OK" && i <= 4)
            {
                result = runCpp(problem, name, raw".\" + problem + raw"\Tests\in" + i + ".txt", raw".\" + problem + raw"\Tests\out" + i + ".txt")
                i += 1
            }
            return result
        }
        else return "CE"
    }

    def main(args: Array[String]):Unit={
        println(judgeScala("Problem1", "scalaTest.scala"))
        println(judgeCpp("Problem1", "cppTest.cpp"))
    }
}