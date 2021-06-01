import scala.sys.process._
import java.io.File
import java.util.Calendar
import scala.collection.immutable._

object Judge{

  /** A helper method for deleting files.*/
  def deleteFile(filename: String) = new File(filename).delete()

  /** Windows won't recognize any commands unless 
      that correction is added to them*/
  def panderToWindows(command: Seq[String]): Seq[String] = {

    val os = sys.props("os.name").toLowerCase
    os match {
      case x if x contains "windows" => Seq("cmd", "/C") ++ command
      case _ => command
    }
  }

  /** Compile the given submission with the machine's Scala compiler.*/
  def compileScala(problem: String, name: String, dir: String): Boolean = {
    val compileCommand = panderToWindows(Seq("fsc", dir + name))
    compileCommand.!
    val isCompiled = new java.io.File("Solution.class").exists
    return isCompiled
  }

  /** Assuming the program has been compiled, run the binary with a given test.
      Returns "OK" if the output is as expected, otherwise returns "WA".
      Note: All codes which are to be tested should be objects which name is Solution!*/
  def runScala(problem: String, name: String, inputFile: String, outputFile: String, dir: String): String = {
 
    val runCommand = panderToWindows(Seq("scala", "Solution"))

    // Extract input and output 
    val inp = Process.cat(new File(inputFile))
    val out = Process.cat(new File(outputFile))

    //Piping the input to the compiled program
    val result = (inp #> runCommand).!!

    //Checking whether the result of the program is equal to the desired output
    if (out.!! == result) "OK" else "WA"
  }

  /** Combine Scala compilation and running 
  tests in a single method*/
  def judgeScala(user: String, problem: String, name: String, tests: Int): String = {

    val dir = raw".\" + problem + raw"\Solutions\" + user + raw"\"
    if (compileScala(problem, name, dir)){

      var i = 1
      var result = "OK"
      while (result == "OK" && i <= tests)
      {
        result = runScala(problem, 
                          name,
                          raw".\" + problem + raw"\Tests\in" + i + ".txt",
                          raw".\" + problem + raw"\Tests\out" + i + ".txt",
                          dir)
        i += 1
      }
      deleteFile("Solution.class")
      deleteFile("Solution$.class")
      return result
    }
    else return "CE"
  }

  /** Compile the given submission with the machine's C++ compiler.*/
  def compileCpp(problem: String, name: String, dir: String): Boolean = {

    // Create a "g++ -o program filename.cpp" process for the command prompt
    val compileCommand = panderToWindows(Seq("g++", "-o", "program", dir + name))

    // Run the process
    compileCommand.!

    // Check if the compilation was successful by checking if a compiled file is present
    val isCompiled = new java.io.File("program.exe").exists
    return isCompiled
  }

  /** Assuming the program has been compiled, run the binary with a given test.
      Returns "OK" if the output is as expected, otherwise returns "WA".*/
  def runCpp(problem: String, name: String, inputFile: String, outputFile: String, dir: String): String = {

    // Create a "program.exe" process for the command prompt
    val runCommand = panderToWindows(Seq("program.exe"))

    // Extract input and output 
    val inp = Process.cat(new File(inputFile))
    val out = Process.cat(new File(outputFile))

    // Piping the input to the compiled program
    val resultOutput = (inp #> runCommand).!!

    if (out.!! == resultOutput) "OK" else "WA"
  }

  /** Combine C++ compilation and running 
      tests in a single method */
  def judgeCpp(user: String, problem: String, name: String, tests: Int): String = {
        
    val dir = raw".\" + problem + raw"\Solutions\" + user + raw"\"

    if (compileCpp(problem, name, dir)){

      var i = 1
      var result = "OK"
      while (result == "OK" && i <= tests){

        result = runCpp(problem,
                        name,
                        raw".\" + problem + raw"\Tests\in" + i + ".txt",
                        raw".\" + problem + raw"\Tests\out" + i + ".txt",
                        dir)
        
        i += 1
      }
      deleteFile("program.exe")
      return result
    }
    else return "CE"
  }

  /** Universal judge method, independent of
      the language of the solution. */
  def judge(user: String, problem: String, filename: String, tests: Int): String = {

    val splitfilename = filename.split('.').toList
    val extension = splitfilename.last
    val result = extension match {
      case "scala" => judgeScala(user, problem, filename, tests)
      case "cpp" => judgeCpp(user, problem, filename, tests)
      case _ => "File not supported"
    }

    val output = 
      Calendar.getInstance().getTime().toString + " " * 5 + 
      user + (" " * (20 - user.length)) +
      problem + (" " * (20 - problem.length)) +
      result

    return output
  }

  def main(args: Array[String]): Unit = {

    println(judge(
                  "user1",
                  "Problem1",
                  "scalaTest.scala",
                  4))

    println(judge(
                  "user2",
                  "Problem1",
                  "cppTest.cpp",
                  4))
  }
}