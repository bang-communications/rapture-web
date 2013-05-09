/**************************************************************************************************
Rapture Web Library
Version 0.8.0

The primary distribution site is

  http://www.propensive.com/

Copyright 2010-2012 Propensive Ltd.

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is
distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing permissions and limitations under the
License.
***************************************************************************************************/
package rapture.web

import rapture.io._

trait Css {

  trait AttType { def string: String }

  case class Stylesheet(rules: Rule*) {
    override def toString = rules.mkString("\n")
  }

  implicit class StringAttType(s: String) extends AttType { def string = s }

  case class Rule(selector: Selector, properties: Properties) {
    override def toString = selector.toString+" "+propBlock
    def propBlock = properties.props.reverse.mkString("{\n  ", ";\n  ", ";\n}\n")
  }

  case class Properties(props: List[Property]) {
    override def toString = props.reverse.map(_.short).mkString(";")
    def +(property: Property) = Properties(property :: props)
    def +(properties: Properties) = Properties(properties.props ::: props)
  }
  implicit def propertyToProperties(prop: Property) = Properties(List(prop))

  class Property(att: String, value: String) {
    override def toString = att+": "+value
    def short = att+":"+value
    def +(property: Property) = Properties(property :: this :: Nil)
    def +(properties: Properties) = Properties(properties.props ::: List(this))
    def unary_! = new Property(att, value+" !important")
  }

  trait CssAttributeStringable[T] { def string(t: T): String }

  implicit val stringStringable = new CssAttributeStringable[String] { def string(t: String) = t }

  class CssAttribute(val att: String) {
    def apply(i: inherit.type): Property = prop("inherit")
    def prop(v: String) = new Property(att, v)
  }

  object inherit

  case class AttributeExtra(string: String)

  implicit class BaseAttributeKeyExtras(bak: Html5#BaseAttributeKey[_, _]) {
    def ~->(v: String) = AttributeExtra(bak.key+"~="+v)
    def |->(v: String) = AttributeExtra(bak.key+"|="+v)
    def ^->(v: String) = AttributeExtra(bak.key+"^=\""+v+"\"")
    def *->(v: String) = AttributeExtra(bak.key+"*=\""+v+"\"")
    def &->(v: String) = AttributeExtra(bak.key+"$=\""+v+"\"")
  }

  case class Selector(name: String) {
    def ~(selector: Selector) = Selector(name+" ~ "+selector.name)
    def >(selector: Selector) = Selector(name+">"+selector.name)
    def /(selector: Selector) = Selector(name+" "+selector.name)
    def +(selector: Selector) = Selector(name+" + "+selector.name)
    def |(selector: Selector) = Selector(name+", "+selector.name)
    def &(selector: Selector) = Selector(name+selector.name)

    def link = Selector(name+":link")
    def visited = Selector(name+":visited")
    def active = Selector(name+":active")
    def hover = Selector(name+":hover")
    def focus = Selector(name+":focus")
    def firstLetter = Selector(name+":first-letter")
    def firstLine = Selector(name+":first-line")
    def firstChild = Selector(name+":first-child")
    def before = Selector(name+":before")
    def after = Selector(name+":after")
    def lang(lang: String) = Selector(name+":lang("+lang+")")
    def firstOfType = Selector(name+":first-of-type")
    def lastOfType = Selector(name+":last-of-type")
    def onlyOfType = Selector(name+":only-of-type")
    def onlyChild = Selector(name+":only-child")
    def nthChild(n: Int)= Selector(name+":nth-child("+n+")")
    def nthLastChild(n: Int)= Selector(name+":nth-last-child("+n+")")
    def nthOfType(n: Int)= Selector(name+":nth-of-type("+n+")")
    def lastChild = Selector(name+":last-child")
    def root = Selector(name+":root")
    def empty = Selector(name+":empty")
    def target = Selector(name+":target")
    def enabled = Selector(name+":enabled")
    def disabled = Selector(name+":disabled")
    def checked = Selector(name+":checked")
    def not(selector: Selector) = Selector("not("+selector.name+")")
    def selection = Selector(name+"::selection")

    def apply(att: Html5#BaseAttributeKey[_, _]): Selector = Selector(name+"["+att.key+"]")
    def apply(att: (Html5#BaseAttributeKey[_, _], String)): Selector = Selector(name+"["+att._1.key+"=\""+att._2+"\"]")
    def apply(att: Html5#Attribute[_]): Selector = Selector(name+"["+att.key+"="+att.value+"]")
    def apply(att: AttributeExtra): Selector = Selector(name+"["+att.string+"]")

    def :=(properties: Properties): Rule = Rule(this, properties)

    override def toString = name
  }

  implicit def symbolToSelector(symbol: Symbol) = Selector("."+symbol.name)
  implicit def stringToSelector(string: String) = Selector("#"+string)
  implicit def htmlToSelector(tag: Html5#Tag[_, _, _]) = Selector(tag.name)

  val * = Selector("*")

}

trait CssTypes extends Css {

  class FixedScroll(val fs: String)
  object fixed extends FixedScroll("fixed")
  object scroll extends FixedScroll("scroll")
  trait FixedScrollVal extends CssAttribute { def apply(v: FixedScroll) = prop(v.fs) }

  class Color(val name: String)
  val black = new Color("black")
  val silver = new Color("silver")
  val gray = new Color("gray")
  val white = new Color("white")
  val maroon = new Color("maroon")
  val red = new Color("red")
  val purple = new Color("purple")
  val fuchsia = new Color("fuchsia")
  val green = new Color("green")
  val lime = new Color("lime")
  val olive = new Color("olive")
  val yellow = new Color("yellow")
  val navy = new Color("navy")
  val blue = new Color("blue")
  val teal = new Color("teal")
  val aqua = new Color("aqua")
  def rgb(r: Int, g: Int, b: Int) = new Color("rgb("+r+", "+g+", "+b+")")
  def hsl(h: Double, s: Double, l: Double) = new Color("hsl("+h+", "+s+", "+l+")")
  def hsla(h: Double, s: Double, l: Double, a: Double) = new Color("hsla("+h+", "+s+", "+l+", "+a+")")
  def rgba(r: Int, g: Int, b: Int, a: Double) = new Color("rgb("+r+", "+g+", "+b+", "+a+")")
  val transparent = new Color("transparent")
  val currentColor = new Color("currentColor")
  trait ColorVal extends CssAttribute { def apply(v: Color) = prop(v.name) }
  trait FourColorVal extends CssAttribute { def apply(v1: Color, v2: Color, v3: Color, v4: Color) = prop(List(v1, v2, v3, v4).map(_.name).mkString(" ")) }

  trait UriVal extends CssAttribute {
    def url(lnk: Link) = "url(\""+lnk.toString+"\")"
    def apply(uri: Link) = prop(url(uri))
  }

  object none extends BorderStyle { def name = "none" }
  trait NoneVal extends CssAttribute { def apply(n: none.type) = prop("none") }
 
  object collapse
  trait CollapseVal extends CssAttribute { def apply(n: collapse.type) = prop("collapse") }
  
  object separate
  trait SeparateVal extends CssAttribute { def apply(n: separate.type) = prop("separate") }

  class Repetition(val name: String)
  val repeat = new Repetition("repeat")
  val repeatX = new Repetition("repeat-x")
  val repeatY = new Repetition("repeat-y")
  val noRepeat = new Repetition("no-repeat")
  trait RepetitionVal extends CssAttribute { def apply(n: Repetition) = prop(n.name) }

  trait TopBottom { def att: String }
  trait LeftRight { def att: String }
  trait VerticalAlign { def att: String }
  
  object center extends LeftRight with TopBottom { def att: String = "center" }

  case class CssLength(s: String)

  implicit class IntLengths(i: Int) {
    def px = CssLength(i+"px")
    def em = CssLength(i+"em")
    def ex = CssLength(i+"ex")
    def in = CssLength(i+"in")
    def cm = CssLength(i+"cm")
    def mm = CssLength(i+"mm")
    def pt = CssLength(i+"pt")
    def pc = CssLength(i+"pc")
  }
  
  implicit class DoubleLengths(d: Double) {
    def em = CssLength(d+"em")
    def ex = CssLength(d+"ex")
    def in = CssLength(d+"in")
    def cm = CssLength(d+"cm")
    def mm = CssLength(d+"mm")
    def pt = CssLength(d+"pt")
    def pc = CssLength(d+"pc")
  }

  trait CssLengthVal extends CssAttribute { def apply(n: CssLength) = prop(n.s) }
  
  type CssPercent = Int => Int
  
  trait CssPercentVal extends CssAttribute { def apply(n: CssPercent) = prop(n(Int.MaxValue)+"%") }

  class BgClip(val name: String)
  val borderBox = new BgClip("border-box")
  val paddingBox = new BgClip("padding-box")
  val contentBox = new BgClip("content-box")
  trait BgClipVal extends CssAttribute { def apply(n: BgClip) = prop(n.name) }

  class BgSize(val name: String)
  val cover = new BgSize("cover")
  val contain = new BgSize("contain")
  trait BgSizeVal extends CssAttribute { def apply(n: BgSize) = prop(n.name) }

  class Thickness(val name: String)
  val thin = new Thickness("thin")
  val medium = new Thickness("medium") with FontSize
  val thick = new Thickness("thick")
  trait ThicknessVal extends CssAttribute { def apply(n: Thickness) = prop(n.name) }

  trait BorderStyle { def name: String }
  val hidden = new BorderStyle { def name = "hidden" }
  val dotted = new BorderStyle { def name = "dotted" }
  val dashed = new BorderStyle { def name = "dashed" }
  val solid = new BorderStyle { def name = "solid" }
  val double = new BorderStyle { def name = "double" }
  val groove = new BorderStyle { def name = "groove" }
  val ridge = new BorderStyle { def name = "ridge" }
  val inset = new BorderStyle { def name = "inset" }
  val outset = new BorderStyle { def name = "outset" }
  trait BorderStyleVal extends CssAttribute { def apply(bs: BorderStyle) = prop(bs.name) }


  trait BorderOptions extends CssAttribute {
    def apply(len: CssLength) = prop(len.s)
    def apply(th: Thickness) = prop(th.name)
    def apply(len: CssLength, st: BorderStyle) = prop(len.s+" "+st.name)
    def apply(th: Thickness, st: BorderStyle) = prop(th.name+" "+st.name)
    def apply(len: CssLength, col: Color) = prop(len.s+" "+col.name)
    def apply(th: Thickness, col: Color) = prop(th.name+" "+col.name)
    def apply(len: CssLength, st: BorderStyle, col: Color) = prop(len.s+" "+st.name+" "+col.name)
    def apply(th: Thickness, st: BorderStyle, col: Color) = prop(th.name+" "+st.name+" "+col.name)
  }

  object auto
  trait AutoVal extends CssAttribute { def apply(a: auto.type) = prop("auto") }

  trait GenericFont { def name: String }
  val serif = new GenericFont { def name = "serif" }
  val sansSerif = new GenericFont { def name = "sans-serif" }
  val cursive = new GenericFont { def name = "cursive" }
  val fantasy = new GenericFont { def name = "fantasy" }
  val monospace = new GenericFont { def name = "monospace" }

  trait FontFamilyVal extends CssAttribute {
    def apply(s: String) = prop("\""+s+"\"")
    def apply(f: GenericFont) = prop(f.name)
  }
  
  trait FontSize { def name: String }
  val xxSmall = new FontSize { def name = "xx-small" }
  val xSmall = new FontSize { def name = "x-small" }
  val small = new FontSize { def name = "small" }
  val smaller = new FontSize { def name = "smaller" }
  val large = new FontSize { def name = "large" }
  val xLarge = new FontSize { def name = "x-large" }
  val xxLarge = new FontSize { def name = "xx-large" }
  val larger = new FontSize { def name = "larger" }
  trait FontSizeVal extends CssAttribute { def apply(f: FontSize) = prop(f.name) }


  trait FontStyle { def name: String }
  object normal
  trait NormalVal extends CssAttribute { def apply(n: normal.type) = prop("normal") }

  val italic = new FontStyle { def name = "italic" }
  val oblique = new FontStyle { def name = "oblique" }
  trait FontStyleVal extends CssAttribute with NormalVal { def apply(f: FontStyle) = prop(f.name) }

  trait FontVariant { def name: String }
  val smallCaps = new FontVariant { def name = "small-caps" }
  trait FontVariantVal extends CssAttribute with NormalVal { def apply(f: FontVariant) = prop(f.name) }

  trait FontWeight { def name: String }
  val bold = new FontWeight { def name = "bold" }
  val bolder = new FontWeight { def name = "bolder" }
  val lighter = new FontWeight { def name = "lighter" }
  trait FontWeightVal extends CssAttribute with NormalVal { def apply(f: FontWeight) = prop(f.name) }
  trait IntVal extends CssAttribute { def apply(i: Int) = prop(i.toString) }
  trait DoubleVal extends CssAttribute { def apply(d: Double) = prop(d.toString) }

  trait FontStretch { def name: String }
  val wider = new FontStretch { def name = "wider" }
  val narrower = new FontStretch { def name = "narrower" }
  val ultraCondensed = new FontStretch { def name = "ultra-condensed" }
  val extraCondensed = new FontStretch { def name = "extra-condensed" }
  val condensed = new FontStretch { def name = "condensed" }
  val semiCondensed = new FontStretch { def name = "semi-condensed" }
  val semiExpanded = new FontStretch { def name = "semi-expanded" }
  val expanded = new FontStretch { def name = "expanded" }
  val extraExpanded = new FontStretch { def name = "extra-expanded" }
  val ultraExpanded = new FontStretch { def name = "ultra-expanded" }
  trait FontStretchVal extends CssAttribute with NormalVal { def apply(s: FontStretch) = prop(s.name) }

  trait ClearOpt { def name: String }
  val both = new ClearOpt { def name = "both" }
  trait ClearOptVal extends CssAttribute { def apply(s: ClearOpt) = prop(s.name) }

  trait Display { def name: String }
  val box = new Display { def name = "box" }
  val flexBox = new Display { def name = "flex-box" }
  val block = new Display { def name = "block" }
  val flex = new Display { def name = "flex" }
  val inline = new Display { def name = "inline" }
  val inlineBlock = new Display { def name = "inline-block" }
  val inlineFlex = new Display { def name = "inline-flex" }
  val inlineTable = new Display { def name = "inline-table" }
  val listItem = new Display { def name = "list-item" }
  val table = new Display { def name = "table" }
  val tableCaption = new Display { def name = "table-caption" }
  val tableCell = new Display { def name = "table-cell" }
  val tableColumn = new Display { def name = "table-column" }
  val tableColumnGroup = new Display { def name = "table-column-group" }
  val tableFooterGroup = new Display { def name = "table-footer-group" }
  val tableHeaderGroup = new Display { def name = "table-header-group" }
  val tableRow = new Display { def name = "table-row" }
  val tableRowGroup = new Display { def name = "table-row-group" }
  trait DisplayVal extends CssAttribute { def apply(d: Display) = prop(d.name) }
  
  trait FloatOpt { def name: String }
  trait FloatOptVal extends CssAttribute { def apply(d: FloatOpt) = prop(d.name) }
  object justify

  trait Decoration { def name: String }
  val underline = new Decoration { def name = "underline" }
  val overline = new Decoration { def name = "overline" }
  val lineThrough = new Decoration { def name = "line-through" }
  val blink = new Decoration { def name = "blink" }
  trait DecorationVal extends CssAttribute { def apply(d: Decoration) = prop(d.name) }
  
  trait TextTransform { def name: String }
  val capitalize = new TextTransform { def name = "capitalize" }
  val uppercase = new TextTransform { def name = "uppercase" }
  val lowercase = new TextTransform { def name = "lowercase" }
  trait TextTransformVal extends CssAttribute { def apply(d: TextTransform) = prop(d.name) }

  val baseline = new VerticalAlign { def att = "baseline" }
  val sub = new VerticalAlign { def att = "sub" }
  val `super` = new VerticalAlign { def att = "super" }
  val textTop = new VerticalAlign { def att = "text-top" }
  val middle = new VerticalAlign { def att = "middle" }
  val textBottom = new VerticalAlign { def att = "text-bottom" }
  trait VerticalAlignVal extends CssAttribute { def apply(v: VerticalAlign) = prop(v.att) }
  
  trait Whitespace { def name: String }
  val nowrap = new Whitespace { def name = "nowrap" }
  val pre = new Whitespace { def name = "pre" }
  val preLine = new Whitespace { def name = "pre-line" }
  val preWrap = new Whitespace { def name = "pre-wrap" }
  trait WhitespaceVal extends CssAttribute { def apply(v: Whitespace) = prop(v.name) }

}

trait Css1 extends CssTypes {
  val background = new CssAttribute("background") with UriVal {
    def apply(lr: LeftRight) = prop(lr.att)
    def apply(tb: TopBottom) = prop(tb.att)
    def apply(len: CssLength) = prop(len.s)
    def apply(pc: CssPercent) = prop(pc(Int.MaxValue)+"%")
    def apply(lr: LeftRight, tb: TopBottom) = prop(lr.att+" "+tb.att)
    def apply(len: CssLength, tb: TopBottom) = prop(len.s+" "+tb.att)
    def apply(pc: CssPercent, tb: TopBottom) = prop(pc(Int.MaxValue)+"% "+tb.att)
    def apply(lr: LeftRight, len: CssLength) = prop(lr.att+" "+len.s)
    def apply(len: CssLength, len2: CssLength) = prop(len.s+" "+len2.s)
    def apply(pc: CssPercent, len: CssLength) = prop(pc(Int.MaxValue)+"% "+len.s)
    def apply(lr: LeftRight, pc: CssPercent) = prop(lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(len: CssLength, pc: CssPercent) = prop(len.s+" "+pc(Int.MaxValue)+"%")
    def apply(pc: CssPercent, pc2: CssPercent) = prop(pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(sc: FixedScroll, lr: LeftRight) = prop(""+sc.fs+" "+lr.att)
    def apply(sc: FixedScroll, tb: TopBottom) = prop(""+sc.fs+" "+tb.att)
    def apply(sc: FixedScroll, len: CssLength) = prop(""+sc.fs+" "+len.s)
    def apply(sc: FixedScroll, pc: CssPercent) = prop(""+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(""+sc.fs+" "+lr.att+" "+tb.att)
    def apply(sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(""+sc.fs+" "+len.s+" "+tb.att)
    def apply(sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(""+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(""+sc.fs+" "+lr.att+" "+len.s)
    def apply(sc: FixedScroll, len: CssLength, len2: CssLength) = prop(""+sc.fs+" "+len.s+" "+len2.s)
    def apply(sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(""+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(""+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(""+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(""+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(rep: Repetition, lr: LeftRight) = prop(rep.name+" "+lr.att)
    def apply(rep: Repetition, tb: TopBottom) = prop(rep.name+" "+tb.att)
    def apply(rep: Repetition, len: CssLength) = prop(rep.name+" "+len.s)
    def apply(rep: Repetition, pc: CssPercent) = prop(rep.name+" "+pc(Int.MaxValue)+"%")
    def apply(rep: Repetition, lr: LeftRight, tb: TopBottom) = prop(rep.name+" "+lr.att+" "+tb.att)
    def apply(rep: Repetition, len: CssLength, tb: TopBottom) = prop(rep.name+" "+len.s+" "+tb.att)
    def apply(rep: Repetition, pc: CssPercent, tb: TopBottom) = prop(rep.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(rep: Repetition, lr: LeftRight, len: CssLength) = prop(rep.name+" "+lr.att+" "+len.s)
    def apply(rep: Repetition, len: CssLength, len2: CssLength) = prop(rep.name+" "+len.s+" "+len2.s)
    def apply(rep: Repetition, pc: CssPercent, len: CssLength) = prop(rep.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(rep: Repetition, lr: LeftRight, pc: CssPercent) = prop(rep.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(rep: Repetition, len: CssLength, pc: CssPercent) = prop(rep.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(rep: Repetition, pc: CssPercent, pc2: CssPercent) = prop(rep.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(rep: Repetition, sc: FixedScroll, lr: LeftRight) = prop(rep.name+" "+sc.fs+" "+lr.att)
    def apply(rep: Repetition, sc: FixedScroll, tb: TopBottom) = prop(rep.name+" "+sc.fs+" "+tb.att)
    def apply(rep: Repetition, sc: FixedScroll, len: CssLength) = prop(rep.name+" "+sc.fs+" "+len.s)
    def apply(rep: Repetition, sc: FixedScroll, pc: CssPercent) = prop(rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(rep: Repetition, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(rep.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(rep: Repetition, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(rep.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(rep: Repetition, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(rep: Repetition, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(rep.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(rep: Repetition, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(rep.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(rep: Repetition, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(rep: Repetition, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(rep.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(rep: Repetition, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(rep.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(rep: Repetition, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(noIm: none.type, lr: LeftRight) = prop("none "+lr.att)
    def apply(noIm: none.type, tb: TopBottom) = prop("none "+tb.att)
    def apply(noIm: none.type, len: CssLength) = prop("none "+len.s)
    def apply(noIm: none.type, pc: CssPercent) = prop("none "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, lr: LeftRight, tb: TopBottom) = prop("none "+lr.att+" "+tb.att)
    def apply(noIm: none.type, len: CssLength, tb: TopBottom) = prop("none "+len.s+" "+tb.att)
    def apply(noIm: none.type, pc: CssPercent, tb: TopBottom) = prop("none "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(noIm: none.type, lr: LeftRight, len: CssLength) = prop("none "+lr.att+" "+len.s)
    def apply(noIm: none.type, len: CssLength, len2: CssLength) = prop("none "+len.s+" "+len2.s)
    def apply(noIm: none.type, pc: CssPercent, len: CssLength) = prop("none "+pc(Int.MaxValue)+"% "+len.s)
    def apply(noIm: none.type, lr: LeftRight, pc: CssPercent) = prop("none "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, len: CssLength, pc: CssPercent) = prop("none "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, pc: CssPercent, pc2: CssPercent) = prop("none "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(noIm: none.type, sc: FixedScroll, lr: LeftRight) = prop("none "+sc.fs+" "+lr.att)
    def apply(noIm: none.type, sc: FixedScroll, tb: TopBottom) = prop("none "+sc.fs+" "+tb.att)
    def apply(noIm: none.type, sc: FixedScroll, len: CssLength) = prop("none "+sc.fs+" "+len.s)
    def apply(noIm: none.type, sc: FixedScroll, pc: CssPercent) = prop("none "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop("none "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(noIm: none.type, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop("none "+sc.fs+" "+len.s+" "+tb.att)
    def apply(noIm: none.type, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop("none "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(noIm: none.type, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop("none "+sc.fs+" "+lr.att+" "+len.s)
    def apply(noIm: none.type, sc: FixedScroll, len: CssLength, len2: CssLength) = prop("none "+sc.fs+" "+len.s+" "+len2.s)
    def apply(noIm: none.type, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop("none "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(noIm: none.type, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop("none "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop("none "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop("none "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, lr: LeftRight) = prop("none "+rep.name+" "+lr.att)
    def apply(noIm: none.type, rep: Repetition, tb: TopBottom) = prop("none "+rep.name+" "+tb.att)
    def apply(noIm: none.type, rep: Repetition, len: CssLength) = prop("none "+rep.name+" "+len.s)
    def apply(noIm: none.type, rep: Repetition, pc: CssPercent) = prop("none "+rep.name+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, lr: LeftRight, tb: TopBottom) = prop("none "+rep.name+" "+lr.att+" "+tb.att)
    def apply(noIm: none.type, rep: Repetition, len: CssLength, tb: TopBottom) = prop("none "+rep.name+" "+len.s+" "+tb.att)
    def apply(noIm: none.type, rep: Repetition, pc: CssPercent, tb: TopBottom) = prop("none "+rep.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(noIm: none.type, rep: Repetition, lr: LeftRight, len: CssLength) = prop("none "+rep.name+" "+lr.att+" "+len.s)
    def apply(noIm: none.type, rep: Repetition, len: CssLength, len2: CssLength) = prop("none "+rep.name+" "+len.s+" "+len2.s)
    def apply(noIm: none.type, rep: Repetition, pc: CssPercent, len: CssLength) = prop("none "+rep.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(noIm: none.type, rep: Repetition, lr: LeftRight, pc: CssPercent) = prop("none "+rep.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, len: CssLength, pc: CssPercent) = prop("none "+rep.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, pc: CssPercent, pc2: CssPercent) = prop("none "+rep.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight) = prop("none "+rep.name+" "+sc.fs+" "+lr.att)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, tb: TopBottom) = prop("none "+rep.name+" "+sc.fs+" "+tb.att)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength) = prop("none "+rep.name+" "+sc.fs+" "+len.s)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent) = prop("none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop("none "+rep.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop("none "+rep.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop("none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop("none "+rep.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength, len2: CssLength) = prop("none "+rep.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop("none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop("none "+rep.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop("none "+rep.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop("none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(uri: Link, lr: LeftRight) = prop(url(uri)+" "+lr.att)
    def apply(uri: Link, tb: TopBottom) = prop(url(uri)+" "+tb.att)
    def apply(uri: Link, len: CssLength) = prop(url(uri)+" "+len.s)
    def apply(uri: Link, pc: CssPercent) = prop(url(uri)+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, lr: LeftRight, tb: TopBottom) = prop(url(uri)+" "+lr.att+" "+tb.att)
    def apply(uri: Link, len: CssLength, tb: TopBottom) = prop(url(uri)+" "+len.s+" "+tb.att)
    def apply(uri: Link, pc: CssPercent, tb: TopBottom) = prop(url(uri)+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(uri: Link, lr: LeftRight, len: CssLength) = prop(url(uri)+" "+lr.att+" "+len.s)
    def apply(uri: Link, len: CssLength, len2: CssLength) = prop(url(uri)+" "+len.s+" "+len2.s)
    def apply(uri: Link, pc: CssPercent, len: CssLength) = prop(url(uri)+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(uri: Link, lr: LeftRight, pc: CssPercent) = prop(url(uri)+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, len: CssLength, pc: CssPercent) = prop(url(uri)+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, pc: CssPercent, pc2: CssPercent) = prop(url(uri)+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(uri: Link, sc: FixedScroll, lr: LeftRight) = prop(url(uri)+" "+sc.fs+" "+lr.att)
    def apply(uri: Link, sc: FixedScroll, tb: TopBottom) = prop(url(uri)+" "+sc.fs+" "+tb.att)
    def apply(uri: Link, sc: FixedScroll, len: CssLength) = prop(url(uri)+" "+sc.fs+" "+len.s)
    def apply(uri: Link, sc: FixedScroll, pc: CssPercent) = prop(url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(url(uri)+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(uri: Link, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(url(uri)+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(uri: Link, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(uri: Link, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(url(uri)+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(uri: Link, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(url(uri)+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(uri: Link, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(uri: Link, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(url(uri)+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(url(uri)+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, lr: LeftRight) = prop(url(uri)+" "+rep.name+" "+lr.att)
    def apply(uri: Link, rep: Repetition, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+tb.att)
    def apply(uri: Link, rep: Repetition, len: CssLength) = prop(url(uri)+" "+rep.name+" "+len.s)
    def apply(uri: Link, rep: Repetition, pc: CssPercent) = prop(url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, lr: LeftRight, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+lr.att+" "+tb.att)
    def apply(uri: Link, rep: Repetition, len: CssLength, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+len.s+" "+tb.att)
    def apply(uri: Link, rep: Repetition, pc: CssPercent, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(uri: Link, rep: Repetition, lr: LeftRight, len: CssLength) = prop(url(uri)+" "+rep.name+" "+lr.att+" "+len.s)
    def apply(uri: Link, rep: Repetition, len: CssLength, len2: CssLength) = prop(url(uri)+" "+rep.name+" "+len.s+" "+len2.s)
    def apply(uri: Link, rep: Repetition, pc: CssPercent, len: CssLength) = prop(url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(uri: Link, rep: Repetition, lr: LeftRight, pc: CssPercent) = prop(url(uri)+" "+rep.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, len: CssLength, pc: CssPercent) = prop(url(uri)+" "+rep.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, pc: CssPercent, pc2: CssPercent) = prop(url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+tb.att)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+len.s)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, lr: LeftRight) = prop(col.name+" "+lr.att)
    def apply(col: Color, tb: TopBottom) = prop(col.name+" "+tb.att)
    def apply(col: Color, len: CssLength) = prop(col.name+" "+len.s)
    def apply(col: Color, pc: CssPercent) = prop(col.name+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+lr.att+" "+tb.att)
    def apply(col: Color, len: CssLength, tb: TopBottom) = prop(col.name+" "+len.s+" "+tb.att)
    def apply(col: Color, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, lr: LeftRight, len: CssLength) = prop(col.name+" "+lr.att+" "+len.s)
    def apply(col: Color, len: CssLength, len2: CssLength) = prop(col.name+" "+len.s+" "+len2.s)
    def apply(col: Color, pc: CssPercent, len: CssLength) = prop(col.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, len: CssLength, pc: CssPercent) = prop(col.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, sc: FixedScroll, lr: LeftRight) = prop(col.name+" "+sc.fs+" "+lr.att)
    def apply(col: Color, sc: FixedScroll, tb: TopBottom) = prop(col.name+" "+sc.fs+" "+tb.att)
    def apply(col: Color, sc: FixedScroll, len: CssLength) = prop(col.name+" "+sc.fs+" "+len.s)
    def apply(col: Color, sc: FixedScroll, pc: CssPercent) = prop(col.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(col: Color, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(col.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(col: Color, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(col.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(col: Color, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(col.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(col: Color, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(col.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(col.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, lr: LeftRight) = prop(col.name+" "+rep.name+" "+lr.att)
    def apply(col: Color, rep: Repetition, tb: TopBottom) = prop(col.name+" "+rep.name+" "+tb.att)
    def apply(col: Color, rep: Repetition, len: CssLength) = prop(col.name+" "+rep.name+" "+len.s)
    def apply(col: Color, rep: Repetition, pc: CssPercent) = prop(col.name+" "+rep.name+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+rep.name+" "+lr.att+" "+tb.att)
    def apply(col: Color, rep: Repetition, len: CssLength, tb: TopBottom) = prop(col.name+" "+rep.name+" "+len.s+" "+tb.att)
    def apply(col: Color, rep: Repetition, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+rep.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, rep: Repetition, lr: LeftRight, len: CssLength) = prop(col.name+" "+rep.name+" "+lr.att+" "+len.s)
    def apply(col: Color, rep: Repetition, len: CssLength, len2: CssLength) = prop(col.name+" "+rep.name+" "+len.s+" "+len2.s)
    def apply(col: Color, rep: Repetition, pc: CssPercent, len: CssLength) = prop(col.name+" "+rep.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, rep: Repetition, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+rep.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, len: CssLength, pc: CssPercent) = prop(col.name+" "+rep.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+rep.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, sc: FixedScroll, lr: LeftRight) = prop(col.name+" "+rep.name+" "+sc.fs+" "+lr.att)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, tb: TopBottom) = prop(col.name+" "+rep.name+" "+sc.fs+" "+tb.att)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, len: CssLength) = prop(col.name+" "+rep.name+" "+sc.fs+" "+len.s)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, pc: CssPercent) = prop(col.name+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+rep.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(col.name+" "+rep.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(col.name+" "+rep.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(col.name+" "+rep.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(col.name+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, rep: Repetition, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+rep.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(col.name+" "+rep.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, rep: Repetition, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, lr: LeftRight) = prop(col.name+" none "+lr.att)
    def apply(col: Color, noIm: none.type, tb: TopBottom) = prop(col.name+" none "+tb.att)
    def apply(col: Color, noIm: none.type, len: CssLength) = prop(col.name+" none "+len.s)
    def apply(col: Color, noIm: none.type, pc: CssPercent) = prop(col.name+" none "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, lr: LeftRight, tb: TopBottom) = prop(col.name+" none "+lr.att+" "+tb.att)
    def apply(col: Color, noIm: none.type, len: CssLength, tb: TopBottom) = prop(col.name+" none "+len.s+" "+tb.att)
    def apply(col: Color, noIm: none.type, pc: CssPercent, tb: TopBottom) = prop(col.name+" none "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, noIm: none.type, lr: LeftRight, len: CssLength) = prop(col.name+" none "+lr.att+" "+len.s)
    def apply(col: Color, noIm: none.type, len: CssLength, len2: CssLength) = prop(col.name+" none "+len.s+" "+len2.s)
    def apply(col: Color, noIm: none.type, pc: CssPercent, len: CssLength) = prop(col.name+" none "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, noIm: none.type, lr: LeftRight, pc: CssPercent) = prop(col.name+" none "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, len: CssLength, pc: CssPercent) = prop(col.name+" none "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, pc: CssPercent, pc2: CssPercent) = prop(col.name+" none "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, sc: FixedScroll, lr: LeftRight) = prop(col.name+" none "+sc.fs+" "+lr.att)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, tb: TopBottom) = prop(col.name+" none "+sc.fs+" "+tb.att)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, len: CssLength) = prop(col.name+" none "+sc.fs+" "+len.s)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, pc: CssPercent) = prop(col.name+" none "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(col.name+" none "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(col.name+" none "+sc.fs+" "+len.s+" "+tb.att)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(col.name+" none "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(col.name+" none "+sc.fs+" "+lr.att+" "+len.s)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(col.name+" none "+sc.fs+" "+len.s+" "+len2.s)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(col.name+" none "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, noIm: none.type, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(col.name+" none "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(col.name+" none "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(col.name+" none "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, lr: LeftRight) = prop(col.name+" none "+rep.name+" "+lr.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, len: CssLength) = prop(col.name+" none "+rep.name+" "+len.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, pc: CssPercent) = prop(col.name+" none "+rep.name+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, lr: LeftRight, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+lr.att+" "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, len: CssLength, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+len.s+" "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, pc: CssPercent, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, lr: LeftRight, len: CssLength) = prop(col.name+" none "+rep.name+" "+lr.att+" "+len.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, len: CssLength, len2: CssLength) = prop(col.name+" none "+rep.name+" "+len.s+" "+len2.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, pc: CssPercent, len: CssLength) = prop(col.name+" none "+rep.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, lr: LeftRight, pc: CssPercent) = prop(col.name+" none "+rep.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, len: CssLength, pc: CssPercent) = prop(col.name+" none "+rep.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, pc: CssPercent, pc2: CssPercent) = prop(col.name+" none "+rep.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+lr.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+len.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(col.name+" none "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, lr: LeftRight) = prop(col.name+" "+url(uri)+" "+lr.att)
    def apply(col: Color, uri: Link, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+tb.att)
    def apply(col: Color, uri: Link, len: CssLength) = prop(col.name+" "+url(uri)+" "+len.s)
    def apply(col: Color, uri: Link, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+lr.att+" "+tb.att)
    def apply(col: Color, uri: Link, len: CssLength, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+len.s+" "+tb.att)
    def apply(col: Color, uri: Link, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, uri: Link, lr: LeftRight, len: CssLength) = prop(col.name+" "+url(uri)+" "+lr.att+" "+len.s)
    def apply(col: Color, uri: Link, len: CssLength, len2: CssLength) = prop(col.name+" "+url(uri)+" "+len.s+" "+len2.s)
    def apply(col: Color, uri: Link, pc: CssPercent, len: CssLength) = prop(col.name+" "+url(uri)+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, uri: Link, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, len: CssLength, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+url(uri)+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, sc: FixedScroll, lr: LeftRight) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+lr.att)
    def apply(col: Color, uri: Link, sc: FixedScroll, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+tb.att)
    def apply(col: Color, uri: Link, sc: FixedScroll, len: CssLength) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+len.s)
    def apply(col: Color, uri: Link, sc: FixedScroll, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(col: Color, uri: Link, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(col: Color, uri: Link, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, uri: Link, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(col: Color, uri: Link, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(col: Color, uri: Link, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, uri: Link, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+url(uri)+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, lr: LeftRight) = prop(col.name+" "+url(uri)+" "+rep.name+" "+lr.att)
    def apply(col: Color, uri: Link, rep: Repetition, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, len: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+len.s)
    def apply(col: Color, uri: Link, rep: Repetition, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+lr.att+" "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, len: CssLength, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+len.s+" "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, lr: LeftRight, len: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+lr.att+" "+len.s)
    def apply(col: Color, uri: Link, rep: Repetition, len: CssLength, len2: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+len.s+" "+len2.s)
    def apply(col: Color, uri: Link, rep: Repetition, pc: CssPercent, len: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, uri: Link, rep: Repetition, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, len: CssLength, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+len.s)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att+" "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+len.s+" "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent, tb: TopBottom) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+tb.att)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight, len: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att+" "+len.s)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength, len2: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+len.s+" "+len2.s)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent, len: CssLength) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+len.s)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, lr: LeftRight, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, len: CssLength, pc: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+len.s+" "+pc(Int.MaxValue)+"%")
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll, pc: CssPercent, pc2: CssPercent) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs+" "+pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
    def apply(sc: FixedScroll) = prop(sc.fs)
    def apply(rep: Repetition) = prop(rep.name)
    def apply(rep: Repetition, sc: FixedScroll) = prop(rep.name+" "+sc.fs)
    def apply(noIm: none.type) = prop("none")
    def apply(noIm: none.type, sc: FixedScroll) = prop("none "+sc.fs)
    def apply(noIm: none.type, rep: Repetition) = prop("none "+rep.name)
    def apply(noIm: none.type, rep: Repetition, sc: FixedScroll) = prop("none "+rep.name+" "+sc.fs)
    def apply(uri: Link, sc: FixedScroll) = prop(url(uri)+" "+sc.fs)
    def apply(uri: Link, rep: Repetition) = prop(url(uri)+" "+rep.name)
    def apply(uri: Link, rep: Repetition, sc: FixedScroll) = prop(url(uri)+" "+rep.name+" "+sc.fs)
    def apply(col: Color) = prop(col.name)
    def apply(col: Color, sc: FixedScroll) = prop(col.name+" "+sc.fs)
    def apply(col: Color, rep: Repetition) = prop(col.name+" "+rep.name)
    def apply(col: Color, rep: Repetition, sc: FixedScroll) = prop(col.name+" "+rep.name+" "+sc.fs)
    def apply(col: Color, noIm: none.type) = prop(col.name+" none")
    def apply(col: Color, noIm: none.type, sc: FixedScroll) = prop(col.name+" none "+sc.fs)
    def apply(col: Color, noIm: none.type, rep: Repetition) = prop(col.name+" none "+rep.name)
    def apply(col: Color, noIm: none.type, rep: Repetition, sc: FixedScroll) = prop(col.name+" none "+rep.name+" "+sc.fs)
    def apply(col: Color, uri: Link) = prop(col.name+" "+url(uri))
    def apply(col: Color, uri: Link, sc: FixedScroll) = prop(col.name+" "+url(uri)+" "+sc.fs)
    def apply(col: Color, uri: Link, rep: Repetition) = prop(col.name+" "+url(uri)+" "+rep.name)
    def apply(col: Color, uri: Link, rep: Repetition, sc: FixedScroll) = prop(col.name+" "+url(uri)+" "+rep.name+" "+sc.fs)
  }
  val backgroundAttachment = new CssAttribute("background-attachment") with FixedScrollVal
  val backgroundColor = new CssAttribute("background-color") with ColorVal
  val backgroundImage = new CssAttribute("background-image") with UriVal with NoneVal
  val backgroundPosition = new CssAttribute("background-position") {
    def apply(lr: LeftRight) = prop(lr.att)
    def apply(tb: TopBottom) = prop(tb.att)
    def apply(len: CssLength) = prop(len.s)
    def apply(pc: CssPercent) = prop(pc(Int.MaxValue)+"%")
    def apply(lr: LeftRight, tb: TopBottom) = prop(lr.att+" "+tb.att)
    def apply(len: CssLength, tb: TopBottom) = prop(len.s+" "+tb.att)
    def apply(pc: CssPercent, tb: TopBottom) = prop(pc(Int.MaxValue)+"% "+tb.att)
    def apply(lr: LeftRight, len: CssLength) = prop(lr.att+" "+len.s)
    def apply(len: CssLength, len2: CssLength) = prop(len.s+" "+len2.s)
    def apply(pc: CssPercent, len: CssLength) = prop(pc(Int.MaxValue)+"% "+len.s)
    def apply(lr: LeftRight, pc: CssPercent) = prop(lr.att+" "+pc(Int.MaxValue)+"%")
    def apply(len: CssLength, pc: CssPercent) = prop(len.s+" "+pc(Int.MaxValue)+"%")
    def apply(pc: CssPercent, pc2: CssPercent) = prop(pc(Int.MaxValue)+"% "+pc2(Int.MaxValue)+"%")
  }
  val backgroundRepeat = new CssAttribute("background-repeat") with RepetitionVal
  
  val backgroundClip = new CssAttribute("background-clip") with BgClipVal
  val backgroundOrigin = new CssAttribute("background-origin") with BgClipVal
  val backgroundSize = new CssAttribute("background-size") with BgSizeVal with CssLengthVal with CssPercentVal

  val border = new CssAttribute("border") with BorderOptions
  val borderBottom = new CssAttribute("border-bottom") with BorderOptions
  val borderBottomColor = new CssAttribute("border-bottom-color") with ColorVal
  val borderBottomStyle = new CssAttribute("border-bottom-style") with BorderStyleVal
  val borderBottomWidth = new CssAttribute("border-bottom-width") with CssLengthVal with ThicknessVal
  val borderColor = new CssAttribute("border-color") with ColorVal with FourColorVal
  val borderLeft = new CssAttribute("border-left") with CssLengthVal with ThicknessVal
  val borderLeftColor = new CssAttribute("border-left-color") with ColorVal
  val borderLeftStyle = new CssAttribute("border-left-style") with BorderStyleVal
  val borderLeftWidth = new CssAttribute("border-left-width") with CssLengthVal with ThicknessVal
  val borderRight = new CssAttribute("border-right") with BorderOptions
  val borderRightColor = new CssAttribute("border-right-color") with ColorVal
  val borderRightStyle = new CssAttribute("border-right-style") with BorderStyleVal
  val borderRightWidth = new CssAttribute("border-right-width") with CssLengthVal with ThicknessVal
  val borderStyle = new CssAttribute("border-style") {
    def apply(bs: BorderStyle) = prop(bs.name)
    def apply(bs: BorderStyle, bs2: BorderStyle) = prop(bs.name+" "+bs2.name)
    def apply(bs: BorderStyle, bs2: BorderStyle, bs3: BorderStyle) = prop(bs.name+" "+bs2.name+" "+bs3.name)
    def apply(bs: BorderStyle, bs2: BorderStyle, bs3: BorderStyle, bs4: BorderStyle) = prop(bs.name+" "+bs2.name+" "+bs3.name+" "+bs4.name)
  }
  val borderTop = new CssAttribute("border-top") with BorderOptions
  val borderTopColor = new CssAttribute("border-top-color") with ColorVal
  val borderTopStyle = new CssAttribute("border-top-style") with BorderStyleVal
  val borderTopWidth = new CssAttribute("border-top-width") with CssLengthVal with ThicknessVal
  val borderWidth = new CssAttribute("border-width") {
    def apply(len: CssLength) = prop(len.s)
    def apply(th: Thickness) = prop(th.name)
    def apply(len: CssLength, len2: CssLength) = prop(len.s+" "+len2.s)
    def apply(len: CssLength, th2: Thickness) = prop(len.s+" "+th2.name)
    def apply(th: Thickness, len2: CssLength) = prop(th.name+" "+len2.s)
    def apply(th: Thickness, th2: Thickness) = prop(th.name+" "+th2.name)
    def apply(len: CssLength, len2: CssLength, len3: CssLength) = prop(len.s+" "+len2.s+" "+len3.s)
    def apply(len: CssLength, len2: CssLength, th3: Thickness) = prop(len.s+" "+len2.s+" "+th3.name)
    def apply(len: CssLength, th2: Thickness, len3: CssLength) = prop(len.s+" "+th2.name+" "+len3.s)
    def apply(len: CssLength, th2: Thickness, th3: Thickness) = prop(len.s+" "+th2.name+" "+th3.name)
    def apply(th: Thickness, len2: CssLength, len3: CssLength) = prop(th.name+" "+len2.s+" "+len3.s)
    def apply(th: Thickness, len2: CssLength, th3: Thickness) = prop(th.name+" "+len2.s+" "+th3.name)
    def apply(th: Thickness, th2: Thickness, len3: CssLength) = prop(th.name+" "+th2.name+" "+len3.s)
    def apply(th: Thickness, th2: Thickness, th3: Thickness) = prop(th.name+" "+th2.name+" "+th3.name)
    
    def apply(len: CssLength, len2: CssLength, len3: CssLength, len4: CssLength) = prop(len.s+" "+len2.s+" "+len3.s+" "+len4.s)
    def apply(len: CssLength, len2: CssLength, len3: CssLength, th4: Thickness) = prop(len.s+" "+len2.s+" "+len3.s+" "+th4.name)
    def apply(len: CssLength, len2: CssLength, th3: Thickness, len4: CssLength) = prop(len.s+" "+len2.s+" "+th3.name+" "+len4.s)
    def apply(len: CssLength, len2: CssLength, th3: Thickness, th4: Thickness) = prop(len.s+" "+len2.s+" "+th3.name+" "+th4.name)
    def apply(len: CssLength, th2: Thickness, len3: CssLength, len4: CssLength) = prop(len.s+" "+th2.name+" "+len3.s+" "+len4.s)
    def apply(len: CssLength, th2: Thickness, len3: CssLength, th4: Thickness) = prop(len.s+" "+th2.name+" "+len3.s+" "+th4.name)
    def apply(len: CssLength, th2: Thickness, th3: Thickness, len4: CssLength) = prop(len.s+" "+th2.name+" "+th3.name+" "+len4.s)
    def apply(len: CssLength, th2: Thickness, th3: Thickness, th4: Thickness) = prop(len.s+" "+th2.name+" "+th3.name+" "+th4.name)
    def apply(th: Thickness, len2: CssLength, len3: CssLength, len4: CssLength) = prop(th.name+" "+len2.s+" "+len3.s+" "+len4.s)
    def apply(th: Thickness, len2: CssLength, len3: CssLength, th4: Thickness) = prop(th.name+" "+len2.s+" "+len3.s+" "+th4.name)
    def apply(th: Thickness, len2: CssLength, th3: Thickness, len4: CssLength) = prop(th.name+" "+len2.s+" "+th3.name+" "+len4.s)
    def apply(th: Thickness, len2: CssLength, th3: Thickness, th4: Thickness) = prop(th.name+" "+len2.s+" "+th3.name+" "+th4.name)
    def apply(th: Thickness, th2: Thickness, len3: CssLength, len4: CssLength) = prop(th.name+" "+th2.name+" "+len3.s+" "+len4.s)
    def apply(th: Thickness, th2: Thickness, len3: CssLength, th4: Thickness) = prop(th.name+" "+th2.name+" "+len3.s+" "+th4.name)
    def apply(th: Thickness, th2: Thickness, th3: Thickness, len4: CssLength) = prop(th.name+" "+th2.name+" "+th3.name+" "+len4.s)
    def apply(th: Thickness, th2: Thickness, th3: Thickness, th4: Thickness) = prop(th.name+" "+th2.name+" "+th3.name+" "+th4.name)
  }

  val height = new CssAttribute("height") with CssLengthVal with CssPercentVal with AutoVal
  val width = new CssAttribute("width") with CssLengthVal with CssPercentVal with AutoVal
  
  // FIXME: implement
  val font = new CssAttribute("font")
  val fontFamily = new CssAttribute("font-family") with FontFamilyVal
  val fontSize = new CssAttribute("font-size") with CssLengthVal with CssPercentVal with FontSizeVal
  val fontStyle = new CssAttribute("font-style") with FontStyleVal
  val fontVariant = new CssAttribute("font-variant") with FontVariantVal
  val fontWeight = new CssAttribute("font-weight") with FontWeightVal with IntVal
  
  val padding = new CssAttribute("padding") with CssLengthVal {
    def apply(len1: CssLength, len2: CssLength) = prop(len1.s+" "+len2.s)
    def apply(len1: CssLength, len2: CssLength, len3: CssLength) = prop(len1.s+" "+len2.s+" "+len3.s)
    def apply(len1: CssLength, len2: CssLength, len3: CssLength, len4: CssLength) = prop(len1.s+" "+len2.s+" "+len3.s+" "+len4.s)
  }
  val paddingBottom = new CssAttribute("padding-bottom") with CssLengthVal
  val paddingLeft = new CssAttribute("padding-left") with CssLengthVal
  val paddingRight = new CssAttribute("padding-right") with CssLengthVal
  val paddingTop = new CssAttribute("padding-top") with CssLengthVal
  val clear = new CssAttribute("clear") with ClearOptVal with NoneVal
  val display = new CssAttribute("display") with DisplayVal with NoneVal
  val float = new CssAttribute("float") with FloatOptVal
  val color = new CssAttribute("color") with ColorVal
  val letterSpacing = new CssAttribute("letter-spacing") with NormalVal with CssLengthVal
  val lineHeight = new CssAttribute("line-height") with NormalVal with CssLengthVal with CssPercentVal
  val textAlign = new CssAttribute("text-align") {
    def apply(j: justify.type) = prop("justify")
    def apply(lr: LeftRight) = prop(lr.att)
  }
  val textDecoration = new CssAttribute("text-decoration") with NoneVal with DecorationVal
  val textIndent = new CssAttribute("text-indent") with CssLengthVal with CssPercentVal
  val textTransform = new CssAttribute("text-transform") with TextTransformVal with NoneVal
  val verticalAlign = new CssAttribute("vertical-align") with VerticalAlignVal
  val whiteSpace = new CssAttribute("white-space") with WhitespaceVal with NormalVal
  val wordSpacing = new CssAttribute("word-spacing") with NormalVal with CssLengthVal
}

trait Css2 extends Css1 {
  val outline = new CssAttribute("outline")
  val outlineColor = new CssAttribute("outline-color")
  val outlineStyle = new CssAttribute("outline-style")
  val outlineWidth = new CssAttribute("outline-width")
  val maxHeight = new CssAttribute("max-height")
  val maxWidth = new CssAttribute("max-width")
  val minHeight = new CssAttribute("min-height")
  val minWidth = new CssAttribute("min-width")
  val content = new CssAttribute("content")
  val counterIncrement = new CssAttribute("counter-increment")
  val counterReset = new CssAttribute("counter-reset")
  val quotes = new CssAttribute("quotes")
  val listStyle = new CssAttribute("list-style")
  val listStyleImage = new CssAttribute("list-style-image")
  val listStylePosition = new CssAttribute("list-style-position")
  val listStyleType = new CssAttribute("list-style-type")
  val margin = new CssAttribute("margin") with CssLengthVal {
    def apply(len1: CssLength, len2: CssLength) = prop(len1.s+" "+len2.s)
    def apply(len1: CssLength, len2: CssLength, len3: CssLength) = prop(len1.s+" "+len2.s+" "+len3.s)
    def apply(len1: CssLength, len2: CssLength, len3: CssLength, len4: CssLength) = prop(len1.s+" "+len2.s+" "+len3.s+" "+len4.s)
  }

  val marginBottom = new CssAttribute("margin-bottom") with CssLengthVal
  val marginLeft = new CssAttribute("margin-left") with CssLengthVal
  val marginRight = new CssAttribute("margin-right") with CssLengthVal
  val marginTop = new CssAttribute("margin-top") with CssLengthVal
  val bottom = new CssAttribute("bottom") with TopBottom with VerticalAlign
  val clip = new CssAttribute("clip")
  val cursor = new CssAttribute("cursor")
  val left = new CssAttribute("left") with LeftRight with ClearOpt with FloatOpt { def name = "left" }
  val overflow = new CssAttribute("overflow")
  val position = new CssAttribute("position")
  val right = new CssAttribute("right") with LeftRight with ClearOpt with FloatOpt { def name = "right" }
  val top = new CssAttribute("top") with TopBottom with VerticalAlign
  val visibility = new CssAttribute("visibility")
  val zIndex = new CssAttribute("z-index")
  val orphans = new CssAttribute("orphans")
  val pageBreakAfter = new CssAttribute("page-break-after")
  val pageBreakBefore = new CssAttribute("page-break-before")
  val pageBreakInside = new CssAttribute("page-break-inside")
  val widows = new CssAttribute("widows")
  val borderCollapse = new CssAttribute("border-collapse")
  val borderSpacing = new CssAttribute("border-spacing")
  val captionSide = new CssAttribute("caption-side")
  val emptyCells = new CssAttribute("empty-cells")
  val tableLayout = new CssAttribute("table-layout")
  val direction = new CssAttribute("direction")
  val unicodeBidi = new CssAttribute("unicode-bidi")
}

trait Css3 extends Css2 {
  val animation = new CssAttribute("animation")
  val animationName = new CssAttribute("animation-name")
  val animationDuration = new CssAttribute("animation-duration")
  val animationTimingFunction = new CssAttribute("animation-timing-function")
  val animationDelay = new CssAttribute("animation-delay")
  val animationIterationCount = new CssAttribute("animation-iteration-count")
  val animationDirection = new CssAttribute("animation-direction")
  val animationPlayState = new CssAttribute("animation-play-state")
  val borderBottomLeftRadius = new CssAttribute("border-bottom-left-radius")
  val borderBottomRightRadius = new CssAttribute("border-bottom-right-radius")
  val borderImage = new CssAttribute("border-image")
  val borderImageOutset = new CssAttribute("border-image-outset")
  val borderImageRepeat = new CssAttribute("border-image-repeat")
  val borderImageSlice = new CssAttribute("border-image-slice")
  val borderImageSource = new CssAttribute("border-image-source")
  val borderImageWidth = new CssAttribute("border-image-width")
  val borderRadius = new CssAttribute("border-radius")
  val borderTopLeftRadius = new CssAttribute("border-top-left-radius")
  val borderTopRightRadius = new CssAttribute("border-top-right-radius")
  val boxDecorationBreak = new CssAttribute("box-decoration-break")
  val boxShadow = new CssAttribute("box-shadow")
  val overflowX = new CssAttribute("overflow-x")
  val overflowY = new CssAttribute("overflow-y")
  val overflowStyle = new CssAttribute("overflow-style")
  val rotation = new CssAttribute("rotation")
  val rotationPoint = new CssAttribute("rotation-point")
  val colorProfile = new CssAttribute("color-profile")
  val opacity = new CssAttribute("opacity")
  val renderingIntent = new CssAttribute("rendering-intent")
  val bookmarkLabel = new CssAttribute("bookmark-label")
  val bookmarkLevel = new CssAttribute("bookmark-level")
  val bookmarkTarget = new CssAttribute("bookmark-target")
  val floatOffset = new CssAttribute("float-offset")
  val hyphenateAfter = new CssAttribute("hyphenate-after")
  val hyphenateBefore = new CssAttribute("hyphenate-before")
  val hyphenateCharacter = new CssAttribute("hyphenate-character")
  val hyphenateLines = new CssAttribute("hyphenate-lines")
  val hyphenateResource = new CssAttribute("hyphenate-resource")
  val hyphens = new CssAttribute("hyphens")
  val imageResolution = new CssAttribute("image-resolution")
  val marks = new CssAttribute("marks")
  val StringSet = new CssAttribute("String-set")
  val boxAlign = new CssAttribute("box-align")
  val boxDirection = new CssAttribute("box-direction")
  val boxFlex = new CssAttribute("box-flex")
  val boxFlexGroup = new CssAttribute("box-flex-group")
  val boxLines = new CssAttribute("box-lines")
  val boxOrdinalGroup = new CssAttribute("box-ordinal-group")
  val boxOrient = new CssAttribute("box-orient")
  val boxPack = new CssAttribute("box-pack")
  val fontSizeAdjust = new CssAttribute("font-size-adjust") with DoubleVal with NoneVal
  val fontStretch = new CssAttribute("font-stretch") with FontStretchVal
  val crop = new CssAttribute("crop")
  val moveTo = new CssAttribute("move-to")
  val pagePolicy = new CssAttribute("page-policy")
  val gridColumns = new CssAttribute("grid-columns")
  val gridRows = new CssAttribute("grid-rows")
  val target = new CssAttribute("target")
  val targetName = new CssAttribute("target-name")
  val targetNew = new CssAttribute("target-new")
  val targetPosition = new CssAttribute("target-position")
  val alignmentAdjust = new CssAttribute("alignment-adjust")
  val alignmentBaseline = new CssAttribute("alignment-baseline")
  val baselineShift = new CssAttribute("baseline-shift")
  val dominantBaseline = new CssAttribute("dominant-baseline")
  val dropInitialAfterAdjust = new CssAttribute("drop-initial-after-adjust")
  val dropInitialAfterAlign = new CssAttribute("drop-initial-after-align")
  val dropInitialBeforeAdjust = new CssAttribute("drop-initial-before-adjust")
  val dropInitialBeforeAlign = new CssAttribute("drop-initial-before-align")
  val dropInitialSize = new CssAttribute("drop-initial-size")
  val dropInitialValue = new CssAttribute("drop-initial-value")
  val inlineBoxAlign = new CssAttribute("inline-box-align")
  val lineStacking = new CssAttribute("line-stacking")
  val lineStackingRuby = new CssAttribute("line-stacking-ruby")
  val lineStackingShift = new CssAttribute("line-stacking-shift")
  val lineStackingStrategy = new CssAttribute("line-stacking-strategy")
  val textHeight = new CssAttribute("text-height")
  val marqueeDirection = new CssAttribute("marquee-direction")
  val marqueePlayCount = new CssAttribute("marquee-play-count")
  val marqueeSpeed = new CssAttribute("marquee-speed")
  val marqueeStyle = new CssAttribute("marquee-style")
  val columnCount = new CssAttribute("column-count")
  val columnFill = new CssAttribute("column-fill")
  val columnGap = new CssAttribute("column-gap")
  val columnRule = new CssAttribute("column-rule")
  val columnRuleColor = new CssAttribute("column-rule-color")
  val columnRuleStyle = new CssAttribute("column-rule-style")
  val columnRuleWidth = new CssAttribute("column-rule-width")
  val columnSpan = new CssAttribute("column-span")
  val columnWidth = new CssAttribute("column-width")
  val columns = new CssAttribute("columns")
  val fit = new CssAttribute("fit")
  val fitPosition = new CssAttribute("fit-position")
  val imageOrientation = new CssAttribute("image-orientation")
  val page = new CssAttribute("page")
  val size = new CssAttribute("size")
  val rubyAlign = new CssAttribute("ruby-align")
  val rubyOverhang = new CssAttribute("ruby-overhang")
  val rubyPosition = new CssAttribute("ruby-position")
  val rubySpan = new CssAttribute("ruby-span")
  val mark = new CssAttribute("mark")
  val markAfter = new CssAttribute("mark-after")
  val markBefore = new CssAttribute("mark-before")
  val phonemes = new CssAttribute("phonemes")
  val rest = new CssAttribute("rest")
  val restAfter = new CssAttribute("rest-after")
  val restBefore = new CssAttribute("rest-before")
  val voiceBalance = new CssAttribute("voice-balance")
  val voiceDuration = new CssAttribute("voice-duration")
  val voicePitch = new CssAttribute("voice-pitch")
  val voicePitchRange = new CssAttribute("voice-pitch-range")
  val voiceRate = new CssAttribute("voice-rate")
  val voiceStress = new CssAttribute("voice-stress")
  val voiceVolume = new CssAttribute("voice-volume")
  val hangingPunctuation = new CssAttribute("hanging-punctuation")
  val punctuationTrim = new CssAttribute("punctuation-trim")
  val textAlignLast = new CssAttribute("text-align-last")
  val textJustify = new CssAttribute("text-justify")
  val textOutline = new CssAttribute("text-outline")
  val textOverflow = new CssAttribute("text-overflow")
  val textShadow = new CssAttribute("text-shadow")
  val textWrap = new CssAttribute("text-wrap")
  val wordBreak = new CssAttribute("word-break")
  val wordWrap = new CssAttribute("word-wrap")
  val transform = new CssAttribute("transform")
  val transformOrigin = new CssAttribute("transform-origin")
  val transformStyle = new CssAttribute("transform-style")
  val perspective = new CssAttribute("perspective")
  val perspectiveOrigin = new CssAttribute("perspective-origin")
  val backfaceVisibility = new CssAttribute("backface-visibility")
  val transition = new CssAttribute("transition")
  val transitionProperty = new CssAttribute("transition-property")
  val transitionDuration = new CssAttribute("transition-duration")
  val transitionTimingFunction = new CssAttribute("transition-timing-function")
  val transitionDelay = new CssAttribute("transition-delay")
  val appearance = new CssAttribute("appearance")
  val boxSizing = new CssAttribute("box-sizing")
  val icon = new CssAttribute("icon")
  val navDown = new CssAttribute("nav-down")
  val navIndex = new CssAttribute("nav-index")
  val navLeft = new CssAttribute("nav-left")
  val navRight = new CssAttribute("nav-right")
  val navUp = new CssAttribute("nav-up")
  val outlineOffset = new CssAttribute("outline-offset")
  val resize = new CssAttribute("resize")
}

object Css extends Css3
