#' 检测用户是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param user_name 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userAdd_isNew()
userAdd_isNew <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='demo') {
sql <- paste0("SELECT  Fuser
  FROM t_md_userRight
  where FappId ='",app_id,"' and  Fuser='",user_name,"'")
data = tsda::sql_select(conn,sql)
ncount =nrow(data)
if(ncount>0){
  res = FALSE
}else{
  res = TRUE
}
return(res)


}


#' 检测用户是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param user_name 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userAdd_isNew()
userAdd_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='demo') {
  sql <- paste0("SELECT
Fuser as  用户名
,fname as 真实姓名
,Fpermissions as 角色
  FROM t_md_userRight
  where FappId ='",app_id,"' and  Fuser='",user_name,"'")
  data = tsda::sql_select(conn,sql)

  return(data)


}


#' 检测用户是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param user_name 用户名
#' @param emp_name 真实姓名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userAdd_save()
userAdd_save <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='demo',emp_name='新增用户') {

  flag = userAdd_isNew(conn = conn,app_id = app_id ,user_name = user_name)
  if(flag){
    sql <- paste0("  insert into t_md_userRight
  SELECT  '",user_name,"'  as Fuser
      ,Fpassword
      ,Fpermissions
      , '",emp_name,"' as Fname
      ,FappId
      ,Fdeleted
      ,FSesstionCount
  FROM  t_md_userRight
  where FappId ='",app_id,"'  and Fuser='demo'")
    tsda::sql_update(conn,sql)
    res = TRUE

  }else{
    res = FALSE
  }
  return(res)

}

#' 用户新增的逻辑处理
#'
#' @param input 输入
#' @param output 输出
#' @param app_id 程序
#' @param session 会话
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userAddServer()
userAddServer <- function(input,output,session,app_id){
  var_userName = tsui::var_text('userAdd_userName_txt')
  var_empName = tsui::var_text('userAdd_EmpName_txt')
  shiny::observeEvent(input$userAdd_upload_btn,{
    user_name = var_userName()
    emp_name =var_empName()
    print(user_name)
    if(user_name ==''){
      tsui::pop_notice("请输入用户名!")
    }else{
      if(emp_name ==''){
        tsui::pop_notice("请输入真实姓名!")

      }else{

        flag = userAdd_save(app_id = app_id,user_name = user_name,emp_name = emp_name)
        data = userAdd_query(app_id = app_id,user_name = user_name)
        tsui::run_dataTable2(id = 'userAdd_query_dataview',data = data)
        if(flag){
          tsui::pop_notice("用户新增成功!")
        }else{
          tsui::pop_notice("用户已经存在!")
        }



      }

    }



  })





}
