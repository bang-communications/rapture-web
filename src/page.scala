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
import rapture.core._
import strategy.throwExceptions

object Layout {

  import MimeTypes._

  trait PageMetadata { page: Page =>
    override def metas: List[HtmlCss.Element[HtmlCss.Metadata]] =
      (metaData.toList map { case (k, v) => HtmlCss.meta(HtmlCss.name -> k, HtmlCss.content -> v)() }) :::
          page.metas
    
    def metaData: Map[Symbol, String] = Map(
      'description -> metaDescription,
      'keywords -> metaKeywords.mkString(","),
      'author -> metaAuthor
    )

    def metaDescription: String
    def metaKeywords: List[String]
    def metaAuthor: String
  }

  trait JQueryUi extends Page { this: JQuery =>
    def jQueryUiLocation = Http / "ajax.googleapis.com" / "ajax" / "libs" / "jqueryui" / "1.8.23" /
        "jquery-ui.min.js"
    
    override def scripts: List[HtmlCss.Element[HtmlCss.Metadata]] =
      HtmlCss.script(HtmlCss.scriptType -> `text/javascript`, HtmlCss.src -> Link(jQueryUiLocation.schemeSpecificPart)) :: super.scripts
  }

  trait JQuery extends Page {

    def jQueryLocation: HttpUrl = Http / "ajax.googleapis.com" / "ajax" / "libs" / "jquery" / "1.7.2" / "jquery.min.js"

    override def scripts: List[HtmlCss.Element[HtmlCss.Metadata]] =
      HtmlCss.script(HtmlCss.scriptType -> `text/javascript`, HtmlCss.src -> Link(jQueryLocation.schemeSpecificPart)) :: super.scripts
  }

  abstract class Page { page =>

    def httpStatus = 200

    def doctype = "<!DOCTYPE html>"

    def stylesheets: List[Stylesheet] = Nil
    case class Stylesheet(link: Link)

    def lang: String = "en"
    def title: String

    def links: List[HtmlCss.Element[HtmlCss.Metadata]] =
      stylesheets map { ss => HtmlCss.link(HtmlCss.rel -> "stylesheet", HtmlCss.href -> ss.link)() }
    
    def scripts: List[HtmlCss.Element[HtmlCss.Metadata]] = Nil
    def styles: List[HtmlCss.Element[HtmlCss.Metadata]] = Nil
    def metas: List[HtmlCss.Element[HtmlCss.Metadata]] = Nil

    def head =
      HtmlCss.title(page.title) :: styles.reverse ::: links.reverse ::: scripts.reverse ::: metas

    def body: List[HtmlCss.Element[HtmlCss.Flow]]

    def document =
      HtmlCss.html(HtmlCss.lang -> page.lang)(
        HtmlCss.head(page.head: _*),
        HtmlCss.body(page.body: _*)
      )

    def stream: Input[Char] = {
      val sb = new StringBuilder
      sb.append(doctype)
      sb.append(document.toString)
      sb.toString.input[Char]
    }
  }

  trait Bootstrap extends Page {
   
    def bootstrapLocation = Http / "twitter.github.com" / "bootstrap" / "1.4.0" / "bootstrap.min.css"

    override def links: List[HtmlCss.Element[HtmlCss.Metadata]] =
      HtmlCss.link(HtmlCss.rel -> "stylesheet", HtmlCss.href -> bootstrapLocation)() :: super.links

  }

  import Forms._
  import HtmlCss._
   
  trait TinyMce extends Page {

    def tinyMceLocation: Link

    override def scripts: List[HtmlCss.Element[HtmlCss.Metadata]] =
      HtmlCss.script(HtmlCss.scriptType -> `text/javascript`, HtmlCss.src -> tinyMceLocation)() :: super.links
 
  }
  
  trait TinyMceForm { this: WebForm =>

    implicit val tinyMceEditorRenderer =
      new Renderer[String, Field[String], HtmlEditor] {
        def render(f: Field[String], w: HtmlEditor) =
          textarea(style -> width(100%%), HtmlCss.name -> f.name, cls -> "mceEditorCustom")(raw(f.fieldValue))
      }
  }
  
}
