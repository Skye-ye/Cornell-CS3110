type student = {
  first_name : string;
  last_name : string;
  gpa : float;
}

let stu = {first_name = "John"; last_name = "Doe"; gpa = 3.5}
let extract_name stu = (stu.first_name, stu.last_name)
let create_stu first_name last_name gpa = {first_name; last_name; gpa}
