package fi.hsci.lucene

import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.tokenattributes.{CharTermAttribute, PositionIncrementAttribute}
import org.apache.lucene.analysis.{Analyzer, AnalyzerWrapper, TokenFilter, TokenStream, Tokenizer}
import org.apache.lucene.util.AttributeSource.State

import scala.annotation.varargs

final class NormalisationFilter(input: TokenStream, preserveOriginal: Boolean = false) extends TokenFilter(input) {

  private val termAtt = addAttribute(classOf[CharTermAttribute])
  private val posIncAttr = addAttribute(classOf[PositionIncrementAttribute])
  private var state: State = _

  override def incrementToken = if (state!=null) {
    restoreState(state)
    posIncAttr.setPositionIncrement(0)
    state = null
    true
  } else if (input.incrementToken) {
    val orig = termAtt.toString
    val normalisedTerm = NormalisationFilter.normalise(orig)
    if (preserveOriginal && orig!=normalisedTerm) state = captureState()
    termAtt.setEmpty()
    termAtt.append(normalisedTerm)
    true
  } else false
}

object NormalisationFilter {

  def tokenTransformer(in: TokenStream, tr: (String) => String): TokenFilter = new TokenFilter(in) {
    private val termAtt = addAttribute(classOf[CharTermAttribute])

    override def incrementToken: Boolean = {
      while (input.incrementToken) {
        val tterm = tr(termAtt.toString)
        if (tterm.nonEmpty) {
          termAtt.setEmpty()
          termAtt.append(tterm)
          return true
        }
      }
      false
    }
  }

  def wrapTokenStream(input: TokenStream, preserveOriginal: Boolean = false) = new NormalisationFilter(input, preserveOriginal)

  def wrapAnalyser(analyser: Analyzer, preserveOriginal: Boolean = false): Analyzer = new AnalyzerWrapper(analyser.getReuseStrategy) {
    override def getWrappedAnalyzer(fieldName: String) = analyser

    override def wrapComponents(fieldName: String, components: TokenStreamComponents) = new TokenStreamComponents(components.getSource,wrapTokenStreamForNormalization(fieldName, components.getTokenStream))
    override def wrapTokenStreamForNormalization(fieldName: String, in: TokenStream) = new NormalisationFilter(in,preserveOriginal)
  }

  def wrapAnalyser(analyser: Analyzer, filter: java.util.function.BiFunction[String,TokenStream,TokenStream]): Analyzer = new AnalyzerWrapper(analyser.getReuseStrategy) {
    override def getWrappedAnalyzer(fieldName: String) = analyser
    override def wrapTokenStreamForNormalization(fieldName: String, in: TokenStream) = filter(fieldName,in)

    override def wrapComponents(fieldName: String, components: TokenStreamComponents) = new TokenStreamComponents(components.getSource,wrapTokenStreamForNormalization(fieldName, components.getTokenStream))
  }

  @varargs def createAnalyser(tokeniser: java.util.function.Function[String,Tokenizer], filters: java.util.function.BiFunction[String,TokenStream,TokenStream]*): Analyzer = new Analyzer {
    override def createComponents(fieldName: String) = {
      val t = tokeniser(fieldName)
      new TokenStreamComponents(t,normalize(fieldName,t))
    }

    override def normalize(fieldName: String, src: TokenStream): TokenStream =
      filters.foldLeft(src)((in,f) => f(fieldName,in))
  }

  @varargs def wrapAnalyser(analyser: Analyzer, filters: java.util.function.BiFunction[String,TokenStream,TokenStream]*): Analyzer = new AnalyzerWrapper(analyser.getReuseStrategy) {
    override def getWrappedAnalyzer(fieldName: String) = analyser
    override def wrapComponents(fieldName: String, components: TokenStreamComponents) = new TokenStreamComponents(components.getSource,wrapTokenStreamForNormalization(fieldName, components.getTokenStream))
    override def wrapTokenStreamForNormalization(fieldName: String, in: TokenStream) = filters.foldLeft(in)((in,f) => f(fieldName,in))
  }

  def normalise(token: String): String = {
    var i = 0
    val ret = new StringBuilder(token.length * 2)
    while (i < token.length) {
      token(i) match {
        case 227 if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ää")
          i += 1
        case 'e' if (i + 2 < token.length) && token(i + 1) == 814 && token(i + 2) == 772 =>
          ret.append("ee")
          i += 2
        case 'i' if (i + 2 < token.length) && token(i + 1) == 815 && token(i + 2) == 772 =>
          ret.append("ii")
          i += 2
        case 'k' if (i + 2 < token.length) && token(i + 1) == 769 && token(i + 2) == 772 =>
          ret.append("kk")
          i += 2
        case 331 if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ng")
          i += 1
        case 'o' if (i + 2 < token.length) && token(i + 1) == 816 && token(i + 2) == 772 =>
          ret.append("oo")
          i += 2
        case 'o' if (i + 2 < token.length) && token(i + 1) == 813 && token(i + 2) == 772 =>
          ret.append("oo")
          i += 2
        case 'p' if (i + 2 < token.length) && token(i + 1) == 769 && token(i + 2) == 772 =>
          ret.append("pp")
          i += 2
        case 'u' if (i + 2 < token.length) && token(i + 1) == 815 && token(i + 2) == 772 =>
          ret.append("uu")
          i += 2
        case 'a' if (i + 2 < token.length) && token(i + 1) == 772 && token(i + 2) == 776 =>
          ret.append("ää")
          i += 2
        case 'ö' if (i + 2 < token.length) && token(i + 1) == 815 && token(i + 2) == 772 =>
          ret.append("öö")
          i += 2
        case 603 if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ää")
          i += 1
        case 'a' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("aa")
          i += 1
        case 'b' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("bb")
          i += 1
        case 'd' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("dd")
          i += 1
        case 'e' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ee")
          i += 1
        case 'g' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("gg")
          i += 1
        case 'h' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("hh")
          i += 1
        case 'i' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ii")
          i += 1
        case 'j' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("jj")
          i += 1
        case 'k' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("kk")
          i += 1
        case 'l' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ll")
          i += 1
        case 'm' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("mm")
          i += 1
        case 'n' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("nn")
          i += 1
        case 'o' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("oo")
          i += 1
        case 'p' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("pp")
          i += 1
        case 'r' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("rr")
          i += 1
        case 's' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ss")
          i += 1
        case 't' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("tt")
          i += 1
        case 'u' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("uu")
          i += 1
        case 'v' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("vv")
          i += 1
        case 252 if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("yy")
          i += 1
        case 'y' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("yy")
          i += 1
        case 'z' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("zz")
          i += 1
        case 'å' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("aa")
          i += 1
        case 'ä' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ää")
          i += 1
        case 'ö' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("öö")
          i += 1
        case 'A' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("aa")
          i += 1
        case 'E' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ee")
          i += 1
        case 'I' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ii")
          i += 1
        case 'O' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("oo")
          i += 1
        case 'U' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("uu")
          i += 1
        case 220 if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("yy")
          i += 1
        case 'Y' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("yy")
          i += 1
        case 'Ä' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("ää")
          i += 1
        case 'Ö' if (i + 1 < token.length) && token(i + 1) == 772 =>
          ret.append("öö")
          i += 1
        case 192 => ret.append('a')
        case 193 => ret.append('a')
        case 194 => ret.append("aa")
        case 195 => ret.append('a')
        case 196 => ret.append('ä')
        case 197 => ret.append('å')
        case 198 => ret.append("ae")
        case 199 => ret.append('c')
        case 200 => ret.append('e')
        case 201 => ret.append('e')
        case 202 => ret.append("ee")
        case 203 => ret.append('e')
        case 204 => ret.append('i')
        case 205 => ret.append('i')
        case 206 => ret.append("ii")
        case 207 => ret.append('i')
        case 208 => ret.append('d')
        case 209 => ret.append('n')
        case 210 => ret.append('o')
        case 211 => ret.append('o')
        case 212 => ret.append("oo")
        case 213 => ret.append('o')
        case 214 => ret.append('ö')
        case 215 =>
        case 216 => ret.append('ö')
        case 217 => ret.append('u')
        case 218 => ret.append('u')
        case 219 => ret.append("uu")
        case 220 => ret.append('y')
        case 221 => ret.append('y')
        case 222 => ret.append('t')
        case 223 => ret.append("ss")
        case 224 => ret.append('a')
        case 225 => ret.append('a')
        case 226 => ret.append("aa")
        case 227 => ret.append('a')
        case 228 => ret.append('ä')
        case 229 => ret.append('å')
        case 230 => ret.append("ae")
        case 231 => ret.append('c')
        case 232 => ret.append('e')
        case 233 => ret.append('e')
        case 234 => ret.append("ee")
        case 235 => ret.append('e')
        case 236 => ret.append('i')
        case 237 => ret.append('i')
        case 238 => ret.append("ii")
        case 239 => ret.append('i')
        case 240 => ret.append('d')
        case 241 => ret.append('n')
        case 242 => ret.append('o')
        case 243 => ret.append('o')
        case 244 => ret.append("oo")
        case 245 => ret.append('o')
        case 246 => ret.append('ö')
        case 247 =>
        case 248 => ret.append('ö')
        case 249 => ret.append('u')
        case 250 => ret.append('u')
        case 251 => ret.append("uu")
        case 252 => ret.append('y')
        case 253 => ret.append('y')
        case 254 => ret.append('t')
        case 255 => ret.append('y')
        case 330 => ret.append('n')
        case 331 if (i + 1 < token.length) && token(i + 1) == 331 =>
          ret.append("ng")
          i += 1
        case 331 => ret.append('n')
        case 339 => ret.append('ö')
        case 383 => ret.append('s')
        case 477 => ret.append('e')
        case 592 => ret.append('a')
        case 603 => ret.append('e')
        case 623 => ret.append("uu")
        case 658 => ret.append('g')
        case 7446 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('ö')
          i += 1
        case 7446 => ret.append('o')
        case 7491 if (i + 2 < token.length) && token(i + 1) == 816 && token(i + 2) == 776 =>
          ret.append('ä')
          i += 2
        case 7491 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('ä')
          i += 1
        case 7491 => ret.append('a')
        case 7495 => ret.append('b')
        case 7496 => ret.append('d')
        case 7497 => ret.append('e')
        case 7501 => ret.append('g')
        case 688 => ret.append('h')
        case 8305 => ret.append('i')
        case 7503 => ret.append('k')
        case 737 => ret.append('l')
        case 7504 => ret.append('m')
        case 8319 => ret.append('n')
        case 7505 => ret.append('n')
        case 7506 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('ö')
          i += 1
        case 7506 => ret.append('o')
        case 7510 => ret.append('p')
        case 691 => ret.append('r')
        case 738 => ret.append('s')
        case 7511 => ret.append('t')
        case 7512 if (i + 2 < token.length) && token(i + 1) == 815 && token(i + 2) == 776 =>
          ret.append('y')
          ret += 2
        case 7512 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('y')
          i += 1
        case 7512 => ret.append('u')
        case 7515 => ret.append('v')
        case 696 => ret.append('y')
        case 7468 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('ä')
          i += 1
        case 7468 => ret.append('a')
        case 7482 => ret.append('n')
        case 7481 => ret.append('m')
        case 8336 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('ä')
          i += 1
        case 8336 => ret.append('a')
        case 8337 => ret.append('e')
        case 7522 => ret.append('i')
        case 8338 if (i + 1 < token.length) && token(i + 1) == 776 =>
          ret.append('ö')
          i += 1
        case 8338 => ret.append('o')
        case 7523 => ret.append('r')
        case 7524 => ret.append('u')
        case 7525 => ret.append('v')

        case 7429 => ret.append('d')
        case 665 => ret.append('b')
        case 610 => ret.append('g')
        case 7434 => ret.append('j')
        case 628 => ret.append('n')

        case 170 =>
        case 183 =>
        case 186 =>
        case 702 =>
        case 703 =>
        case 704 =>
        case 8224 =>
        case 8304 =>
        case 8333 =>
        case 8968 =>
        case 8969 =>
        case 8970 =>
        case 8971 =>

        case 1040 => ret.append('a')
        case 1041 => ret.append('b')
        case 1042 => ret.append('v')
        case 1043 => ret.append('g')
        case 1044 => ret.append('d')
        case 1045 => ret.append('e')
        case 1046 =>
        case 1047 =>
        case 1048 => ret.append('i')
        case 1049 =>
        case 1050 => ret.append('k')
        case 1051 => ret.append('j')
        case 1052 => ret.append('m')
        case 1053 => ret.append('n')
        case 1054 => ret.append('o')
        case 1055 => ret.append('p')
        case 1056 => ret.append('r')
        case 1057 => ret.append('s')
        case 1058 => ret.append('t')
        case 1059 => ret.append('u')
        case 1060 =>
        case 1061 => ret.append('h')
        case 1062 => ret.append("ts")
        case 1063 => ret.append("ts")
        case 1064 =>
        case 1065 =>
        case 1066 =>
        case 1067 =>
        case 1068 =>
        case 1069 => ret.append('e')
        case 1070 =>
        case 1071 =>
        case 1072 => ret.append('a')
        case 1073 => ret.append('b')
        case 1074 => ret.append('v')
        case 1075 => ret.append('g')
        case 1076 => ret.append('d')
        case 1077 => ret.append('e')
        case 1078 => ret.append('z')
        case 1079 => ret.append('z')
        case 1080 => ret.append('i')
        case 1081 => ret.append('i')
        case 1082 => ret.append('k')
        case 1083 => ret.append('l')
        case 1084 => ret.append('m')
        case 1085 => ret.append('n')
        case 1086 => ret.append('o')
        case 1087 => ret.append('p')
        case 1088 => ret.append('r')
        case 1089 => ret.append('s')
        case 1090 => ret.append('t')
        case 1091 => ret.append('u')
        case 1092 => ret.append('f')
        case 1093 => ret.append('h')
        case 1094 => ret.append("ts")
        case 1095 => ret.append("ts")
        case 1096 => ret.append('s')
        case 1097 => ret.append("sts")
        case 1098 =>
        case 1099 => ret.append('y')
        case 1100 =>
        case 1101 => ret.append('e')
        case 1102 => ret.append("ju")
        case 1103 => ret.append("ja")
        case 1104 =>
        case 1105 => ret.append("jo")
        case 1106 =>
        case 1107 =>
        case 1108 =>
        case 1109 =>
        case 1110 => ret.append('i')
        case 1111 =>
        case 1112 =>
        case 1113 => ret.append("lj")
        case 1114 =>
        case 1115 =>
        case 1116 =>
        case 1117 =>
        case 1118 =>
        case 1119 =>
        case 1120 =>
        case 1121 =>
        case 1122 => ret.append('e')
        case 1123 => ret.append('e')
        case 936 => ret.append("ps")
        case 945 => ret.append('a')
        case 946 => ret.append('b')
        case 947 => ret.append('g')
        case 948 => ret.append('d')
        case 949 => ret.append('e')
        case 950 => ret.append('z')
        case 951 => ret.append('e')
        case 953 => ret.append('i')
        case 954 => ret.append('k')
        case 955 => ret.append('l')
        case 957 => ret.append('n')
        case 958 => ret.append("ks")
        case 959 => ret.append('o')
        case 960 => ret.append('p')
        case 961 => ret.append('r')
        case 963 => ret.append('s')
        case 964 => ret.append('t')
        case 965 => ret.append('y')
        case 966 => ret.append('f')
        case 967 => ret.append("kh")
        case 968 => ret.append("ps")
        case 977 => ret.append("th")
        case 1008 => ret.append('k')
        // Díakriittiset merkit
        case 768 =>
        case 769 =>
        case 770 =>
        case 771 =>
        case 773 =>
        case 774 =>
        case 775 =>
        case 776 =>
        case 777 =>
        case 778 =>
        case 779 =>
        case 780 =>
        case 781 =>
        case 782 =>
        case 783 =>
        case 784 =>
        case 785 =>
        case 786 =>
        case 787 =>
        case 788 =>
        case 789 =>
        case 790 =>
        case 791 =>
        case 792 =>
        case 793 =>
        case 794 =>
        case 795 =>
        case 796 =>
        case 797 =>
        case 798 =>
        case 799 =>
        case 800 =>
        case 801 =>
        case 802 =>
        case 803 =>
        case 804 =>
        case 805 =>
        case 806 =>
        case 807 =>
        case 808 =>
        case 809 =>
        case 810 =>
        case 811 =>
        case 812 =>
        case 813 =>
        case 814 =>
        case 815 =>
        case 816 =>
        case 817 =>
        case 818 =>
        case 819 =>
        case 820 =>
        case 821 =>
        case 822 =>
        case 823 =>
        case 824 =>
        case 825 =>
        case 826 =>
        case 827 =>
        case 828 =>
        case 829 =>
        case 830 =>
        case 831 =>
        case 832 =>
        case 833 =>
        case 834 =>
        case 835 =>
        case 836 =>
        case 837 =>
        case 838 =>
        case 839 =>
        case 840 =>
        case 841 =>
        case 842 =>
        case 843 =>
        case 844 =>
        case 845 =>
        case 846 =>
        case 847 =>
        case 848 =>
        case 849 =>
        case 850 =>
        case 851 =>
        case 852 =>
        case 853 =>
        case 854 =>
        case 855 =>
        case 856 =>
        case 857 =>
        case 858 =>
        case 859 =>
        case 860 =>
        case 861 =>
        case 862 =>
        case 863 =>
        case 864 =>
        case 865 =>
        case 866 =>
        case 867 =>
        case 868 =>
        case 869 =>
        case 870 =>
        case 871 =>
        case 872 =>
        case 873 =>
        case 874 =>
        case 875 =>
        case 876 =>
        case 877 =>
        case 878 =>
        case 879 =>
        case other => ret.append(other)
      }
      i += 1
    }
    ret.toString
  }
}
