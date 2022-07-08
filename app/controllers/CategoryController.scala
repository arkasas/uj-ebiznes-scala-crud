package controllers

import model.Category
import model.NewCategory
import javax.inject._
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import scala.collection.mutable

@Singleton
class CategoryController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  private val categories = new mutable.ListBuffer[Category]()
  categories += Category(1, "AGD")
  categories += Category(2, "RTF")
  implicit val categoriesListJson = Json.format[Category]
  implicit val newTodoListJson = Json.format[NewCategory]

  def getAll(): Action[AnyContent] = Action {
    if (categories.isEmpty) NoContent else Ok(Json.toJson(categories))
  }

  def getById(itemId: Long) = Action {
    val foundItem = categories.find(_.id == itemId)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def deleteById(itemId: Long) = Action {
    val foundItem = categories.filterInPlace(_.id != itemId)
    Accepted
  }

  def updateName(itemId: Long) = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val newItem: Option[NewCategory] =
      jsonObject.flatMap(
        Json.fromJson[NewCategory](_).asOpt
      )

    val index = categories.indexWhere(el => el.id == itemId)


    newItem match {
      case Some(item) =>
        var current = categories(index)
        categories(index) = Category(current.id, item.name)
        Created(Json.toJson(categories(index)))
      case None => NotFound
    }

  }

  def addNewItem() = Action { implicit request =>
      val content = request.body
      val jsonObject = content.asJson

      val todoListItem: Option[NewCategory] =
        jsonObject.flatMap(
          Json.fromJson[NewCategory](_).asOpt
        )

      todoListItem match {
        case Some(newItem) =>
          val nextId = categories.map(_.id).max + 1
          val toBeAdded = Category(nextId, newItem.name)
          categories += toBeAdded
          Created(Json.toJson(toBeAdded))
        case None =>
          BadRequest
      }
    }

}
