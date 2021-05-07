package scala2021.jatadjanov.task05

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object HW {

  case class Employee(id: Int, name: String, departmentId: Int)
  case class Department(id: Int, name: String)
  case class Manager(department: String, employeeId: Int)
  case class Info(employee: String, department: String, manager: String)
  val employees = List(
    Employee(1, "Steve", 1),
    Employee(3, "Mark", 1),
    Employee(4, "Jane", 1),
    Employee(7, "Samuel", 2),
    Employee(10, "Igor", 2),
    Employee(11, "Naveen", 4),
    Employee(12, "Christy", 5),
    Employee(15, "Megan", 3)
  )
  val departments = List(
    Department(1, "Marketing"),
    Department(2, "Sales"),
    Department(3, "Research"),
    Department(4, "IT"),
  )
  val managers = List(
    Manager("Marketing", 1),
    Manager("Sales", 10),
    Manager("IT", 14),
  )

  def findEmployeeDepartmentIdByName(employee: String): Either[String, Int] = {
    val emp = employees.find(p => p.name == employee)
    if (emp.nonEmpty) {
      Right(emp.get.departmentId)
    } else {
      Left(s"Employee By Name: $employee not found")
    }
  }

  def findEmployeeDepartmentByName(employee: String): Either[String, Department] = {
    val emp = employees.find(p => p.name == employee)
    if (emp.nonEmpty) {
      getDepartmentById(emp.get.departmentId)
    } else {
      Left(s"Employee By Name: $employee not found")
    }
  }

  def getDepartmentById(departmentId: Int) =  {
    val department = departments.find(d => d.id == departmentId)
    if (department.nonEmpty) {
      Right(department.get)
    } else {
      Left(s"Department By Id: $departmentId not found")
    }
  }

  def getDepartmentNameById(departmentId: Int): Either[String, String] =  {
    val department = departments.find(d => d.id == departmentId)
    if (department.nonEmpty) {
      Right(department.get.name)
    } else {
      Left(s"Department By Id: $departmentId not found")
    }
  }

  def getManagerNameByDepartmentName(departmentName: String): Either[String, String] = {
    val manager = managers.find(m => m.department == departmentName)
    if (manager.isEmpty) {
      Left(s"Manager by Department: $departmentName not found")
    } else {
      val emp = employees.find(e => e.id == manager.get.employeeId)
      if (emp.nonEmpty) {
        Right(emp.get.name)
      } else {
        Left(s"Employee by Id: $manager.get.employeeId not found")
      }
    }
  }

  // Найти имя менеджера департамента, в котором работает сотрудник по имени сотрудника
  def findManagerName(employee: String): Option[String] = {
    val r = for {
      emp <- employees if emp.name == employee
      dep <- departments if dep.id == emp.departmentId
      man <- managers if man.department == dep.name
      owner <- employees if owner.id == man.employeeId
    } yield owner.name

    r.nonEmpty match {
      case true => Some(r.head)
      case _ => None
    }
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так
  def findManagerNameOrError(employee: String): Either[String, String] = {
    for {
      emp <- HW.findEmployeeDepartmentIdByName(employee)
      dep <- HW.getDepartmentNameById(emp)
      man <- HW.getManagerNameByDepartmentName(dep)
    } yield man
  }

  val NOT_FOUND: String = "Not Found"

  def getInfoByEmployeeName(employee: String): Info = {
    findEmployeeDepartmentByName(employee) match {
      case Left(_) => Info(employee, NOT_FOUND, NOT_FOUND)
      case Right(department) =>
        getManagerNameByDepartmentName(department.name) match {
          case Left(_) => Info(employee, department.name, NOT_FOUND)
          case Right(managerName) => Info(employee, department.name, managerName)
        }
    }
  }

  // вывести список всех сотрудников, вместе с именем департамента и именем менеджера, если департамента или менеджера нет то использовать константу "Not Found"
  def findEmployeeManagers: List[Info] = {
    employees.map(e => getInfoByEmployeeName(e.name))
  }

  // Найти имя менеджера по имени сотрудника, в случае ошибки в данных - указать что именно не так и сделать все это асинхронно
  def findManagerNameOrErrorAsync(employee: String): Future[Either[String, String]] = Future {
    findManagerNameOrError(employee)
  }

  def main(args: Array[String]): Unit = {
    println(findManagerName("John"))
    println(findManagerNameOrError("Steve"))
    println(findManagerNameOrErrorAsync("Mark"))
    println(findEmployeeManagers)
    println("John: " + findManagerName("John"))
    println("Steve: " + findManagerName("Steve"))
    println("Mark: " + findManagerName("Mark"))
    println("Igor: " + findManagerName("Igor"))
    println("Christy: " + findManagerName("Christy"))
    println("Naveen: " + findManagerName("Naveen"))
    println("Megan: " + findManagerName("Megan"))

  }
}
