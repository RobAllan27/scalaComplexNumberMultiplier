import Array._
import java.io._
import scala.io.Source
import Array.ofDim
import scala.collection.immutable._

object ComplexNumberMatrixMultiplier {
   def main(args: Array[String]): Unit = {


    // Lets initialise the start of the project with values 
    val firstMatrixFilename: String =  "inputComplexNumbersSourceA.txt";
	val secondMatrixFilename: String =  "inputComplexNumbersSourceB.txt";
	val outputMatrixFilename: String =  "outputComplexNumbersSource.txt";
    var numberofRowsInFirstFile: Int = 0
	var numberofColsInFirstFile: Int = 0
	var numberofRowsInSecondFile: Int = 0
	var numberofColsInSecondFile: Int = 0
	val matrixFileDelimiter: Char = ',';
   
	// Let calculate some of the input data sizing
    numberofRowsInFirstFile = fileRowCounter(firstMatrixFilename)
	numberofColsInFirstFile = fileColCounter(firstMatrixFilename,matrixFileDelimiter)
	numberofRowsInSecondFile = fileRowCounter(secondMatrixFilename)
	numberofColsInSecondFile = fileColCounter(secondMatrixFilename,matrixFileDelimiter)
	
	try {
      verifyMatrixSizesAreCompatibleforMultiplication(numberofColsInFirstFile, numberofRowsInSecondFile)
    } catch {
      case e: MyMatrixFilesIncorrectlySizedException =>	println("**** Incorrectly sized matrices **********")
    } finally {
      // This block will always be invoked
      println("Completed Sizing Checks")
    }
	
	// Lets now set up the matrices -  for the files.
	var myFirstMatrixOfComplexNums =Array.ofDim[ComplexNumber](numberofRowsInFirstFile,numberofColsInFirstFile)
	var mySecondMatrixOfComplexNums =Array.ofDim[ComplexNumber](numberofRowsInSecondFile,numberofColsInSecondFile)
	//var myFirstMatrixOfComplexNums =Array[Array[ComplexNumber]](numberofRowsInFirstFile,numberofColsInFirstFile)
	//var mySecondMatrixOfComplexNums =Array[Array[ComplexNumber]](numberofRowsInSecondFile,numberofColsInSecondFile)
	
	// They get updated in the method so create new ones
	var myFirstMatrixOfComplexNumsWhendataFilled = populateMatrixOfComplexNumsFromFile(numberofRowsInFirstFile,numberofColsInFirstFile, firstMatrixFilename, matrixFileDelimiter, myFirstMatrixOfComplexNums)
	var mySecondMatrixOfComplexNumsWhendataFilled = populateMatrixOfComplexNumsFromFile(numberofRowsInSecondFile,numberofColsInSecondFile, secondMatrixFilename, matrixFileDelimiter, mySecondMatrixOfComplexNums)
	
	/* now lets multiply the matrices - some basic maths reminder
	https://www.mathsisfun.com/algebra/matrix-multiplying.html
	m rows in first matrix
	p cols in second matrix
	m*p output matrix
	*/

	var producedComplexNumbersMatrix = multiplyComplexNumMatrices(numberofRowsInFirstFile, numberofColsInFirstFile, numberofRowsInSecondFile , numberofColsInSecondFile, myFirstMatrixOfComplexNumsWhendataFilled, mySecondMatrixOfComplexNumsWhendataFilled)
	
	writeComplexNumbersToFile(producedComplexNumbersMatrix, outputMatrixFilename)
	
   }
   
   def writeComplexNumbersToFile(producedComplexNumbersMatrix: Array[Array[ComplexNumber]], outputMatrixFilename: String): Unit = {
      val file = new File(outputMatrixFilename)
	  val bufferedWriter = new BufferedWriter(new FileWriter(file))
	  for (i <- 0 to producedComplexNumbersMatrix.length - 1 ) {
		  var lineString  = ""
          for (j <- 0 to producedComplexNumbersMatrix(i).length - 1) { // generalised and find the length - allows method to support ragged arrays
              lineString = lineString + producedComplexNumbersMatrix(i)(j)
			  lineString = lineString + ","
		  }		  
		// trim the lat unnecessary ,
		lineString = lineString.dropRight(1)
	    bufferedWriter.write(lineString + "\n")	
	  }
	  bufferedWriter.close()
	  println("Results Have been Written out")
    }

   // Simple method to verify that files are the right shape.
   def verifyMatrixSizesAreCompatibleforMultiplication(numberofColsInFirstFile: Int, numberofRowsInSecondFile: Int): Unit = {
		if (numberofColsInFirstFile != numberofRowsInSecondFile) throw new MyMatrixFilesIncorrectlySizedException
   }

   // Method multiply arrays of complex numbers 
   def multiplyComplexNumMatrices(numberofRowsInFirstFile: Int, numberofColsInFirstFile: Int,numberofRowsInSecondFile: Int, numberofColsInSecondFile: Int, firstMatrixofComplexNumber: Array[Array[ComplexNumber]], secondMatrixofComplexNumber: Array[Array[ComplexNumber]]): Array[Array[ComplexNumber]] = {
		var returnedMultipliedMatrixofComplexNumber = Array.ofDim[ComplexNumber](numberofRowsInFirstFile,numberofColsInSecondFile)
		// possible refactor to use the length for (i <- 0 until a.length) {
		  println("Executing the matrix Multiplication")

		  for (i <- 0 to numberofRowsInFirstFile - 1) {

          for (j <- 0 to numberofColsInSecondFile - 1) {
		  
			var complexNumberAtPositioninArray = new ComplexNumber(0.0,0.0)

		  // now lets iterate multiplying the complex numbers
		     for (k <- 0 to numberofColsInFirstFile - 1){
			 var foundComplexNumberinFirstArray = firstMatrixofComplexNumber(i)(k)
			 var foundComplexNumberinSecondArray = secondMatrixofComplexNumber(k)(j)
			 var multipliedProductTwoComplexNumbers = foundComplexNumberinFirstArray.*(foundComplexNumberinSecondArray)
			 complexNumberAtPositioninArray = complexNumberAtPositioninArray.+(multipliedProductTwoComplexNumbers)
			 }

			 returnedMultipliedMatrixofComplexNumber(i)(j) = complexNumberAtPositioninArray
          }
		}
		return returnedMultipliedMatrixofComplexNumber 
      }
   
   // method to open a file and parse in file and craete a matrix of complex numbers
   def populateMatrixOfComplexNumsFromFile(numberOfRowsInFile: Int, numberOfColsInFile: Int, inputFileName:String , delimiter:Char, matrixofComplexNumber: Array[Array[ComplexNumber]]): Array[Array[ComplexNumber]] = {
       var updatedMatrixofComplexNumber = Array.ofDim[ComplexNumber](numberOfRowsInFile,numberOfColsInFile)
	   println("Populating the Array")
	   var rowInFile: Int = 0;

	   for (line <- Source.fromFile(inputFileName).getLines()) {
		 var complexNumbersinLine=line.split(",")
		 var colInRow: Int = 0;

		 for(singleComplexNumberAsString <- complexNumbersinLine){
		   var newComplexNumberToAdd = parseStringToComplexNumber(singleComplexNumberAsString)
		   updatedMatrixofComplexNumber(rowInFile)(colInRow) = newComplexNumberToAdd;
		   colInRow += 1
         }		 
		 
		 rowInFile += 1
       }
	return updatedMatrixofComplexNumber   
   }
   
   // method to take a string and create a complex number - allows for various formats
    def parseStringToComplexNumber(incomingString: String): ComplexNumber = {
	  // check if it is pure real number - simplest case
	 if(incomingString.indexOf('j') < 0){ 
			var complexNumber = new ComplexNumber(incomingString.toDouble,0.0)
			return complexNumber
      }
	  
	  // Now lets find the last index of a plus or minus
	    var lastIndexofPositiveSign: Int  = incomingString.lastIndexOf('+')
		var lastIndexofNegativeSign: Int  = incomingString.lastIndexOf('-')
		val positionOfSplitinComplexNumber: Int = (lastIndexofPositiveSign).max(lastIndexofNegativeSign)

	  val lastintCharacterBeforeJ: Int = incomingString.length-2
	  // we may not have a REAL part here
	  if (positionOfSplitinComplexNumber == 0){
		var imaginaryPartAsString: String = incomingString.slice(0, lastintCharacterBeforeJ + 1);
		var complexNumber = new ComplexNumber(0.0,imaginaryPartAsString.toDouble)
		return complexNumber
	  }
	  
	  var realPartAsString: String = incomingString.slice(0,positionOfSplitinComplexNumber);
	  var imaginaryPartAsString: String = incomingString.slice(positionOfSplitinComplexNumber, lastintCharacterBeforeJ + 1);
		var complexNumber = new ComplexNumber(realPartAsString.toDouble,imaginaryPartAsString.toDouble)
		return complexNumber
    }
   
	// Method to open a file and obtain the number of Rows
    def fileRowCounter(fileName:String): Int = {
      //println("Getting the row count for a file ... " + fileName)
	  var numberofRowsInFile = Source.fromFile(fileName).getLines().size
	  return numberofRowsInFile;
    }
	
	// Method to open a file and obtain the number of Columns - we assume regular shaped non ragged arrays
    def fileColCounter(fileName:String , delimiter:Char): Int = {
      //println("Getting the count for a file ... " + fileName + " with delimiter " + delimiter)
	
	val fileFirstLine: String = {
		val src = Source.fromFile(fileName)
		val lineList = src.getLines().take(1).toList
		src.close
		lineList.head
      }
	  var numberofColsInFile = fileFirstLine.split(delimiter).length
      return numberofColsInFile	
   }
   
   object adder {
     def addInt( a:Int, b:Int ) : Int = {
       var sum:Int = 0
       sum = a + b
       return sum
    }
  }
  

// Complex number Class -  with hel[er matheamtical operations and toString()  
  case class ComplexNumber(real: Double = 0.0, imaginary: Double = 0.0) {

    def +(other: ComplexNumber) = ComplexNumber(real + other.real, imaginary + other.imaginary)
    def -(other: ComplexNumber) = ComplexNumber(real - other.real, imaginary - other.imaginary)
    def *(other: ComplexNumber) = ComplexNumber(real * other.real - imaginary * other.imaginary, imaginary * other.real +   real * other.imaginary)
    def /(other: ComplexNumber): ComplexNumber = {
      val denominator = other.real * other.real + other.imaginary * other.imaginary
      val divReal = (real * other.real + imaginary * other.imaginary) / denominator
	  val divImaginary = (imaginary * other.real - real * other.imaginary) / denominator 
    ComplexNumber(divReal, divImaginary)
	}
	// Overriding tostring method 
    override def toString() : String = {
		var sign :String = "+"
		if (imaginary < 0){
			sign = "" // no sign for the imaginary part
		}
		return s"$real$sign$imaginary" + "j"
	}
  }	 

  class MyMatrixFilesIncorrectlySizedException extends Exception
}