import scala.sys.process._
import java.io.File

object Judge{

    def compileScala(problem: String, name: String): Boolean=
    {
        // Create a "fsc filename.scala" process for the command prompt
        val compile = Seq("fsc", name)

        // The "fsc" command won't be recognized on windows if that piece of code is not included
        val os = sys.props("os.name").toLowerCase
        val panderToWindows = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ compile
            case _ => compile
        }

        // Run the process
        panderToWindows.!

        // Check if the compilation was successful by checking if a compiled file is present
        val compiled = new java.io.File("Solution.class").exists
        return compiled
    }

    def runScala(problem: String, name: String, inputFile: String, outputFile: String): String ={
        // Note: All codes which are to be tested should be objects which name is Solution
        // Create a "fsc filename.scala" process for the command prompt
        val run = Seq("scala", "Solution")

        // Extract input and output 
        val inp = Process.cat(new File(inputFile))
        val out = Process.cat(new File(outputFile))

        //The "scala" command won't be recognized on windows if that piece of code is not included
        val os = sys.props("os.name").toLowerCase
        val panderToWindows = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ run
            case _ => run
        }

        //Piping the input to the compiled program
        val result = (inp #> panderToWindows).!!

        //Checking whether the result of the program is equal to the desired output
        if(out.!! == result) return "OK"
        else return "WA"
    }

    def judgeScala(problem: String, name: String, tests: Int): String={
        //Combining compileScala and runScala functions
        //CE means compilation was unsuccessful
        //OK means all tests are passed
        //WA means one of the tests didn't pass
        if(compileScala(problem, name))
        {
            var i = 1
            var result = "OK"
            while(result == "OK" && i <= tests)
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
        //Create a "g++ -o program filename.cpp" process for the command prompt
        val compile = Seq("g++", "-o", "program", name)

        //The "g++" command won't be recognized on windows if that piece of code is not included
        val os = sys.props("os.name").toLowerCase
        val panderToWindows = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ compile
            case _ => compile
        }

        //Run the process
        panderToWindows.!

        // Check if the compilation was successful by checking if a compiled file is present
        val compiled = new java.io.File("program.exe").exists
        return compiled
    }

    def runCpp(problem: String, name: String, inputFile: String, outputFile: String): String ={
        // Note: All codes which are to be tested should be objects which name is Solution
        // Create a "fsc filename.scala" process for the command prompt
        val run = Seq(raw".\program.exe")

        // Extract input and output 
        val inp = Process.cat(new File(inputFile))
        val out = Process.cat(new File(outputFile))

        //The running command may not be recognized on windows if that piece of code is not included
        val os = sys.props("os.name").toLowerCase
        val panderToWindows1 = os match {
            case x if x contains "windows" => Seq("cmd", "/C") ++ run
            case _ => run
        }

        //Piping the input to the compiled program
        val result = (inp #> panderToWindows1).!!

        //Checking whether the result of the program is equal to the desired output
        if(out.!! == result) return "OK"
        else return "WA"
    }

    def judgeCpp(problem: String, name: String, tests: Int): String={

        //Combining compileCpp and runCpp functions
        if(compileCpp(problem, name))
        {
            var i = 1
            var result = "OK"
            while(result == "OK" && i <= tests)
            {
                result = runCpp(problem, name, raw".\" + problem + raw"\Tests\in" + i + ".txt", raw".\" + problem + raw"\Tests\out" + i + ".txt")
                i += 1
            }
            return result
        }
        else return "CE"
    }

    def main(args: Array[String]):Unit={
        println(judgeScala("Problem1", "scalaTest.scala", 4))
        println(judgeCpp("Problem1", "cppTest.cpp", 4))
    }
}