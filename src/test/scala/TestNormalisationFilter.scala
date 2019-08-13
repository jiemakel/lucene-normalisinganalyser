import fi.hsci.lucene.ScandinavianCharacterPreservingASCIIFoldingFilter
import fi.hsci.lucene.NormalisationFilter
import org.apache.lucene.analysis.{Analyzer, AnalyzerWrapper, TokenStream}
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.junit.Test
import org.junit.Assert._

class TestNormalisationFilter {
  
  @Test
  def testNormalisationFilter: Unit = {
    val a = NormalisationFilter.wrapAnalyser(new WhitespaceAnalyzer())
    assertEquals("äe",a.normalize("","äe").utf8ToString())
    assertEquals("ae",a.normalize("","áe").utf8ToString())
    assertEquals("ae",a.normalize("","ãe").utf8ToString())
  }

  @Test
  def testScandinavianCharacterPreservingASCIIFoldingFilter: Unit = {
    val a = new AnalyzerWrapper(Analyzer.GLOBAL_REUSE_STRATEGY) {
      val analyser = new WhitespaceAnalyzer()
      override def getWrappedAnalyzer(fieldName: String) = analyser

      override def wrapTokenStreamForNormalization(fieldName: String, in: TokenStream) = new ScandinavianCharacterPreservingASCIIFoldingFilter(in)
    }
    assertEquals("äe",a.normalize("","äe").utf8ToString())
    assertEquals("ae",a.normalize("","áe").utf8ToString())
    assertEquals("ae",a.normalize("","ãe").utf8ToString())
  }
  
}