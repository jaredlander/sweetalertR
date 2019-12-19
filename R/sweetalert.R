#' @title sweetalert
#' @description sweetalert in R as an htmlwidget
#' @details Create sweetalerts in R with this function.  For details on the arguments, see the
#' \href{http://t4t5.github.io/sweetalert/}{SweetAlert demo}.
#' @examples
#' \dontrun{
#' library(sweetalertR)
#' library(htmltools)
#'
#'
#' # do the first sweetalert example using a tagList
#' #  and sweetalert with a selector
#' #  This is the advisable way for most R users
#' browsable(tagList(
#'   HTML('
#'    <div class="showcase sweet">
#'      <h4>Sweet Alert</h4>
#'      <button>Show error message</button>
#'
#'      <h5>R Code:</h5>
#'      <pre>
#'        sweetalert(
#'         selector = ".showcase.sweet button"
#'         ,title = "Oops..."
#'         ,text =  "Something went wrong!"
#'         ,type = "error"
#'        )
#'      </pre>
#'    </div>
#'   ')
#'   ,sweetalert(
#'     selector = '.showcase.sweet button'
#'     ,title = "Oops..."
#'     ,text =  "Something went wrong!"
#'     ,type = "error"
#'   )
#' ))
#'
#' # do the first sweetalert example using a tagList
#' #   but with sweetalert( selector = NULL )
#' #   assuming someone knows HTML and JavaScript
#' browsable(tagList(
#' HTML('
#' <div class="showcase sweet">
#' 	<h4>Sweet Alert</h4>
#' 	<button onclick = "sweetAlert( \'Oops...\',\'Something went wrong!\',\'error\')">
#'     Show error message
#'   </button>
#'
#' 	<h5>R Code:</h5>
#' 	<pre>
#'   # no selector so sweetalert htmlwidget just used for dependencies
#'   sweetalert()
#'   </pre>
#' </div>
#'   ')
#'   ,sweetalert()
#' ))
#' }
#' @export
#' @param selector CSS element selector
#' @param event Event that triggers popup
#' @param title The title of the modal. It can either be added to the object under the key "title" or passed as the first parameter of the function.
#' @param text A description for the modal. It can either be added to the object under the key "text" or passed as the second parameter of the function.
#' @param type The type of the modal. SweetAlert comes with 4 built-in types which will show a corresponding icon animation: "warning", "error", "success" and "info". You can also set it as "input" to get a prompt modal. It can either be put in the object under the key "type" or passed as the third parameter of the function.
#' @param allowEscapeKey If set to true, the user can dismiss the modal by pressing the Escape key.
#' @param customClass A custom CSS class for the modal. It can be added to the object under the key "customClass".
#' @param allowOutsideClick If set to true, the user can dismiss the modal by clicking outside it.
#' @param showCancelButton If set to true, a "Cancel"-button will be shown, which the user can click on to dismiss the modal.
#' @param showConfirmButton If set to false, the "OK/Confirm"-button will be hidden. Make sure you set a timer or set allowOutsideClick to true when using this, in order not to annoy the user.
#' @param confirmButtonText Use this to change the text on the "Confirm"-button. If showCancelButton is set as true, the confirm button will automatically show "Confirm" instead of "OK".
#' @param confirmButtonColor Use this to change the background color of the "Confirm"-button (must be a HEX value).
#' @param cancelButtonText Use this to change the text on the "Cancel"-button.
#' @param closeOnConfirm Set to false if you want the modal to stay open even if the user presses the "Confirm"-button. This is especially useful if the function attached to the "Confirm"-button is another SweetAlert.
#' @param closeOnCancel Same as closeOnConfirm, but for the cancel button.
#' @param imageUrl Add a customized icon for the modal. Should contain a string with the path to the image.
#' @param imageSize If imageUrl is set, you can specify imageSize to describes how big you want the icon to be in px. Pass in a string with two values separated by an "x". The first value is the width, the second is the height.
#' @param timer Auto close timer of the modal. Set in ms (milliseconds).
#' @param html If set to true, will not escape title and text parameters. (Set to false if you’re worried about XSS attacks.)
#' @param animation If set to false, the modal’s animation will be disabled.
#' @param inputType Change the type of the input field when using type: "input" (this can be useful if you want users to type in their password for example).
#' @param inputPlaceholder When using the input-type, you can specify a placeholder to help the user.
#' @param inputValue Specify a default text value that you want your input to show when using type: "input"
#' @param evalFunction JavaScript function for evaluation
#' @param width Width of widget
#' @param height Height of widget
#' @param elementId Use an explicit element ID for the widget (rather than an automatically generated one). Useful if you have other JavaScript that needs to explicitly discover and interact with a specific widget instance.
#'
sweetalert <- function(
  selector = NULL
  , event = 'onclick'
  , title = ''
  , text = ''
  , type = NULL
  , allowOutsideClick = FALSE
  , showConfirmButton = TRUE
  , showCancelButton = FALSE
  , closeOnConfirm = TRUE
  , closeOnCancel = TRUE
  , confirmButtonText = 'OK'
  , confirmButtonColor = '#AEDEF4'
  , cancelButtonText = 'Cancel'
  , imageUrl = NULL
  , imageSize = NULL
  , timer = NULL
  , customClass = ''
  , html = FALSE
  , animation = TRUE
  , allowEscapeKey = TRUE
  , inputType = 'text'
  , inputPlaceholder = ''
  , inputValue = ''
  , evalFunction = NULL
  , width = 0
  , height = 0
  , elementId=NULL
)
{
  # create an elementId if it doesn't exist
  if(is.null(elementId))
  {
    elementId <- paste0(sample(c(letters, LETTERS, 0:9), 10, replace=TRUE), collapse="")
  }

  # convert evalFunction to htmlwidget::JS if text
  if( !is.null(evalFunction) && !inherits(evalFunction,"JS_EVAL") )
    evalFunction = htmlwidgets::JS(evalFunction)

  # forward options using x
  x = list(
    selector = selector
    ,event = event
    ,options = list(
      title = title
      , text = text
      , type = type
      , allowOutsideClick = allowOutsideClick
      , showConfirmButton = showConfirmButton
      , showCancelButton = showCancelButton
      , closeOnConfirm = closeOnConfirm
      , closeOnCancel = closeOnCancel
      , confirmButtonText = confirmButtonText
      , confirmButtonColor = confirmButtonColor
      , cancelButtonText = cancelButtonText
      , imageUrl = imageUrl
      , imageSize = imageSize
      , timer = timer
      , customClass = customClass
      , html = html
      , animation = animation
      , allowEscapeKey = allowEscapeKey
      , inputType = inputType
      , inputPlaceholder = inputPlaceholder
      , inputValue = inputValue
    )
    , evalFunction = evalFunction
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'sweetalert',
    x,
    width = width,
    height = height,
    package = 'sweetalertR',
    elementId=elementId
  )
}
