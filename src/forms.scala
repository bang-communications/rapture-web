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

import scala.language.higherKinds

import rapture.io._
import rapture.time._
import rapture.core._
import rapture.net._

import scala.collection.mutable.ListBuffer

object Forms extends Widgets with Parsers {

  class BasicForm(val name: Symbol, val params: Map[String, String] = Map(),
      val uploads: Map[String, Array[Byte]] = Map()) { form =>
    
    type Field[T] <: BasicField[T]
    
    val formName = name.name
    protected val fields = new ListBuffer[Field[_]]

    def submitted = params.contains(formName+"_submit")
    def complete = submitted
    def save() = fields.foreach(_.save())

    trait BasicField[T] {
      def name: Symbol
      def fieldName: String = name.name
      def paramValue: Option[String] = form.params.get(fieldName)
      
      def value: Option[T] =
        if(parser.submitted(stringValue)) Some(parser.parse(stringValue, dataValue)) else None
      
      def dataValue: Option[Array[Byte]] = form.uploads.get(fieldName)

      def stringValue: Option[String] =
        if(form.submitted) paramValue else if(cell == null) None else parser.serialize(cell())
      
      def fieldValue: String = stringValue.getOrElse("")
      def apply(): T = value.get
      def parser: FieldParser[T]
      def cell: Cell[T]
      def save(): Unit = value foreach { c => if(cell == null) () else cell.update(c) }
    
      override def toString = s"${fieldName}: value=${value}"
    }
  }

  trait FieldLabels {
    type FormField[T] <: LabelledField
    trait LabelledField { def label: String }
  }

  trait FormValidation { this: (BasicForm with FormValidation) =>
    
    def validated = fields.forall(_.validated)
    def showValidation = submitted && !validated

    abstract override def complete = submitted && validated

    type Field[T] <: ValidatedField[T]
    trait ValidatedField[T] { this: BasicField[T] =>
      lazy val validationIssues: List[String] =
        if(!submitted) Nil else validator(stringValue)
      def validated: Boolean = validationIssues.isEmpty
      def validator: Option[String] => List[String]
      def required: Boolean
      
      override def toString = s"${fieldName}: value=${value}, validated=${validated}, required=${required}, validator=${validator}, parser=${parser}, cell=${cell}, paramValue=${paramValue}"
    }
 
    // String validators

    // FIXME: Reformat these lines
    val validUrl: Option[String] => List[String] = {
      case None => Nil
      case Some(s) => if(s.matches("\\b(https?|ftp)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#"+
          "/%=~_|]")) Nil else List("Please enter a valid URL")
    }

    val validInteger: Option[String] => List[String] = {
      case None => Nil
      case Some(s) => if(s.matches("^-?[1-9][0-9]*$")) Nil else List("Please enter a valid number")
    }

    val validPhoneNumber: Option[String] => List[String] = {
      case Some(s) => if(s.matches("""^[+\- ()0-9]*$""")) Nil else List("Please enter a valid tele"+
          "phone number")
      case None => Nil
    }
    
    val validEmailAddress: Option[String] => List[String] = {
      case Some(s) => if(s.matches("""^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$""")) Nil else List("Please enter a valid email address")
      case None => Nil
    }
    
    val optValidEmailAddress: Option[String] => List[String] = {
      case Some(s) => if(s.matches("""^([_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-"+
        "9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4}))?$""")) Nil else List("Please enter a valid email addre"+
        "ss")
      case None => Nil
    }
    
    val validDateTime: Option[String] => List[String] = {
      case Some(s) => if(s.matches("[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9] [0-9][0-"+
        "9]:[0-9][0-9]:[0-9][0-9]")) Nil else List("Please enter a valid date, in the format DD/MM"+
        "/YY hh:mm:ss.")
      case None => Nil
    }
    
    def notEmpty(msg: String = "Value is required and can't be empty."): Option[String] => List[String] = {
      case Some(s) => if(s.isEmpty) List(msg) else Nil
      case None => Nil
    }
    
    val isSlug: Option[String] => List[String] = {
      case Some(s) => if(!s.matches("[a-z0-9]*")) List("Value can only contain lower-case alphanum"+
         "eric characters.") else Nil
      case None => Nil
    }
   
    val isChecked: Option[String] => List[String] = {
      case Some(s) => Nil
      case None => List("You must check this field to continue.")
    }
    
    def notDuplicate(xs: List[String]): Option[String] => List[String] = {
      case Some(s) => if(xs contains s) List("This value is not unique. Please choose something di"+
          "fferent.") else Nil
      case None => Nil
    }
  }

  trait FormHelp { this: BasicForm =>
    type Field[T] <: HelpField[T]
    trait HelpField[T] extends BasicField[T] { def help: String }
  }

  trait Preprocessing { this: BasicForm =>

    type Field[T] <: PreprocessedField[T]

    trait PreprocessedField[T] extends BasicField[T] {
      def processString(s: String): String
      override def stringValue = super.stringValue.map(processString)
    }
  }

  /** Adds renderability functionality to a form.  */
  trait RenderableForm { this: (BasicForm with RenderableForm) =>
    type Field[T] <: RenderableField[T]
    type RenderType
    type FormPart
    type RenderedForm
    val formParts = new ListBuffer[FormPart]

    def wrap[T, F <: Field[T], W <: Widget](field: F, widget: W)
        (implicit renderer: Renderer[T, F, W]): FormPart

    def content(fp: FormPart) = formParts += fp

    def render: RenderedForm

    // asInstanceOf[F] is here as an indirect consequence of compiler bug SI-6443
    trait RenderableField[T] { this: Field[T] =>
      def as[F <: Field[T], W <: Widget](w: W)(implicit renderer:
          Renderer[T, F, W]): this.type = {
        formParts += wrap[T, F, W](this.asInstanceOf[F], w)(renderer)
        fields += this
        this
      }
    }

    trait Renderer[T, -F <: RenderableField[T], -W <: Widget] {
      def render(f: F, w: W): RenderType
      def hideLabel: Boolean = false
    }
  }

  abstract class WebForm(name: Symbol, params: Map[String, String] = Map(),
      uploads: Map[String, Array[Byte]] = Map(),
      val method: HttpMethods.FormMethod = HttpMethods.Post, val action: Link = ^) extends
      BasicForm(name, params, uploads) with RenderableForm with FieldLabels with Preprocessing with
      FormValidation with FormHelp {
    
    def encType: MimeTypes.MimeType =
      if(fields.exists(_.needsMultipart)) MimeTypes.`multipart/form-data`
      else MimeTypes.`application/x-www-form-urlencoded`


    class Field[T](val name: Symbol, val label: String, val cell: Cell[T], val parser: FieldParser[T],
        process: String => String, validate: Option[String] => List[String], val required: Boolean,
        val help: String, val needsMultipart: Boolean = false) extends BasicField[T] with
        RenderableField[T] with LabelledField with PreprocessedField[T] with ValidatedField[T]
        with HelpField[T] {
      def processString(s: String) = process(s)
      def validator = validate
    }

    def field[T: FieldParser](name: Symbol, label: String, cell: Cell[T] = null,
        process: (String => String) = identity[String], validate: Option[String] => List[String] = { s =>
        Nil }, required: Boolean = false, help: String = "") =
      new Field[T](name, label, cell, ?[FieldParser[T]], process, validate, required, help,
          ?[FieldParser[T]].needsMultipart)
    
    import HtmlCss._

    type RenderType = HtmlCss.Element[Phrasing]

    implicit val stringRenderer = new Renderer[String, Field[String], StringInput] {
      def render(f: Field[String], w: StringInput): HtmlCss.Element[HtmlCss.Phrasing] =
        input(HtmlCss.name -> f.name, `type` -> text, HtmlCss.value -> f.fieldValue)
    }

    implicit val passwordRenderer = new Renderer[String, Field[String], PasswordInput] {
      def render(f: Field[String], w: PasswordInput): HtmlCss.Element[HtmlCss.Phrasing] =
        input(HtmlCss.name -> f.name, `type` -> password, HtmlCss.value -> f.fieldValue)
    }

    implicit val uploadRenderer = new Renderer[Array[Byte], Field[Array[Byte]], FileUploader] {
      def render(f: Field[Array[Byte]], w: FileUploader): HtmlCss.Element[HtmlCss.Phrasing] =
        input(HtmlCss.name -> f.name, HtmlCss.`type` -> HtmlCss.file, HtmlCss.value -> f.fieldValue)
    }

    implicit val checkboxRenderer = new Renderer[Boolean, Field[Boolean], Checkbox] {
      override def hideLabel = true
      def render(f: Field[Boolean], w: Checkbox): HtmlCss.Element[HtmlCss.Phrasing] =
        label(
          input(HtmlCss.`type` -> checkbox, HtmlCss.value -> "1", HtmlCss.name -> f.name,
              if(f.value.getOrElse(false)) Some(checked) else None),
          " "+f.label
        )
    }

    implicit val textareaRenderer = new Renderer[String, Field[String], TextArea] {
      def render(f: Field[String], w: TextArea): HtmlCss.Element[HtmlCss.Phrasing] =
        textarea(HtmlCss.name -> f.name, w.maxLength.map(HtmlCss.maxlength -> _))(f.fieldValue)
    }
    
    implicit def dropdownRenderer[T, Q] = new Renderer[T, Field[T], Dropdown[Q]] {
      def render(f: Field[T], w: Dropdown[Q]): HtmlCss.Element[HtmlCss.Phrasing] =
        select(HtmlCss.name -> f.name)(
          w.options map { opt => option(value -> w.id(opt))(w.description(opt)) }
        )
    }
    
    implicit def radioListRenderer[T, Q] = new Renderer[T, Field[T], RadioList[Q]] {
      def render(f: Field[T], w: RadioList[Q]): HtmlCss.Element[HtmlCss.Phrasing] =
        span(style -> display(inlineBlock))(
          w.options flatMap { opt => List(
            span(
              input(`type` -> radio, HtmlCss.name -> f.name, value -> w.id(opt),
                  if(w.id(opt) == f.fieldValue) Some(checked) else None),
              " "+w.description(opt),
              br
            )
          ) }
        )
    }

    implicit val hiddenRenderer = new Renderer[String, Field[String], Hidden] {
      def render(f: Field[String], w: Hidden): HtmlCss.Element[HtmlCss.Phrasing] =
        input(HtmlCss.`type` -> HtmlCss.hidden, HtmlCss.name -> f.name, HtmlCss.value -> f.fieldValue)
    }
  }

  class BootstrapForm(name: Symbol, params: Map[String, String],
      uploads: Map[String, Array[Byte]] = Map(), method: HttpMethods.FormMethod = HttpMethods.Post,
      action: Link = ^) extends WebForm(name, params, uploads, method, action) with FormValidation {
    import HtmlCss._

    type FormPart = Element[Flow]
    type RenderedForm = Element[Flow]

    def hideLabels = false

    def wrap[T, F <: Field[T], W <: Widget](field: F, widget: W)
        (implicit renderer: Renderer[T, F, W]): FormPart =
      div(cls -> ("control-group"+(if(!field.validationIssues.isEmpty) " error" else "")))(
        label(cls -> "control-label"/*, forName -> field.name.name*/)(field.label),
        div(cls -> "controls")(
          renderer.render(field, widget), " ",
          field.validationIssues map { i => span(cls -> "help-inline")(i+" ") }
        )
      )
  
    def render: RenderedForm =
      form(enctype -> encType, cls -> "form", HtmlCss.action -> action, HtmlCss.method -> method)(
        fieldset(
          formParts.toList,
          submitRow
        )
      )

    def submitRow = div(input(HtmlCss.name -> Symbol(formName+"_submit"), `type` -> submit, cls -> "btn btn-primary", value -> submitButtonText))

    def submitButtonText = "Save"
  }

  trait TabularLayout { this: (WebForm with TabularLayout) =>

    import HtmlCss._

    type FormPart = Element[TrItems]
    type RenderedForm = Element[Flow]

    def hideLabels = false

    def wrap[T, F <: Field[T], W <: Widget](field: F, widget: W)
        (implicit renderer: Renderer[T, F, W]): FormPart =
      tr(
        if(hideLabels) Nil
        else if(renderer.hideLabel) List(td(""), td(""))
        else List(
          td(field.label),
          td(if(field.required) "*" else "")
        ),
        td(renderer.render(field, widget))
      )

    def render: RenderedForm =
      form(enctype -> encType, HtmlCss.action -> action, HtmlCss.method -> method)(
        table(
          tbody(
            formParts.toList,
            submitRow
          )
        )
      )
   
    def submitButtonText = "Save"

    def submitRow = if(hideLabels) tr(td(submitButton)) else tr(
      td,
      td,
      td(submitButton)
    )

    def submitButton: Element[Flow] =
      input(HtmlCss.name -> Symbol(formName+"_submit"), value -> submitButtonText, `type` -> submit)

  }
}

trait Parsers {
  @annotation.implicitNotFound("Unable to use values of type ${Value} in form fields without a corresponding FieldParser.")
  trait FieldParser[Value] {
    def parse(value: Option[String], data: Option[Array[Byte]] = None): Value
    def serialize(value: Value): Option[String]
    def submitted(value: Option[String]): Boolean = value.isDefined
    def needsMultipart: Boolean = false
  }

  implicit val StringParser = new FieldParser[String] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = s.getOrElse("")
    def serialize(s: String): Option[String] = Some(s)
  }

  implicit val IntParser = new FieldParser[Int] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None): Int = s.get.toInt
    def serialize(value: Int) = Some(value.toString)
  }

  implicit val BooleanParser = new FieldParser[Boolean] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = s.isDefined
    def serialize(value: Boolean) = if(value) Some("") else None
    override def submitted(value: Option[String]): Boolean = true
  }

  implicit val DataParser = new FieldParser[Array[Byte]] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = data.getOrElse(Array[Byte]())
    def serialize(value: Array[Byte]) = Some("")
    override def needsMultipart: Boolean = true
  }

  def enumParser(enum: Enumeration) = new FieldParser[enum.Value] {
    def parse(s: Option[String], data: Option[Array[Byte]] = None) = enum(s.get.toInt)
    def serialize(value: enum.Value) = Some(value.id.toString)
  }

}

