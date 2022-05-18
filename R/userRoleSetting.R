#' 查询用户
#'
#' @param conn 连接
#' @param app_id 程序ID
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userRoleSetting_user_query()
userRoleSetting_user_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms') {
  sql <- paste0("SELECT   Fuser
  FROM [rdbe].[dbo].[t_md_userRight]
  where FappId ='",app_id,"' and Fdeleted =0
  and Fuser not in ('admin','demo')")
  data =  tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount>0){
    res = data$Fuser
    names(res) <-res
    return(res)
  }else{
    res =''
    names(res) ='无用户'
  }

  return(res)

}


#' 查询用户
#'
#' @param conn 连接
#' @param app_id 程序ID
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userRoleSetting_user_query()
userRoleSetting_role_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms') {
  sql <- paste0("  select FRoleName from t_md_role
  where FappId ='",app_id,"' and Fdeleted =0")
  data =  tsda::sql_select(conn,sql)
  ncount = nrow(data)
  if(ncount>0){
    res = data$FRoleName
    names(res) <-res
    return(res)
  }else{
    res =''
    names(res) ='无角色'
  }

  return(res)

}



#' 查询用户
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param user_name 用户名
#' @param role_name  密码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userRoleSetting_user_query()
userRoleSetting_role_update <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='韩向松',role_name='admin') {
  sql <- paste0("update a set     a.Fpermissions  ='",role_name,"'
  FROM  t_md_userRight a
  where a.FappId ='",app_id,"' and a.Fdeleted =0
  and a.Fuser ='",user_name,"'  ")
  print(sql)
  res =  tsda::sql_update(conn,sql)

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
#' userRoleSettingServer()
userRoleSettingServer <- function(input,output,session,app_id){

  shiny::observeEvent(input$userRoleSetting_getUserName_btn,{

    output$userRoleSetting_userName_ph <- shiny::renderUI({
      shiny::selectInput(inputId = 'userRoleSetting_userName_sel',label = '请选择用户名',choices =userRoleSetting_user_query(app_id = app_id) ,selected =userRoleSetting_user_query(app_id = app_id)[1] ,multiple = FALSE,selectize = TRUE)

    })

    output$userRoleSetting_roleName_ph <- shiny::renderUI({
      shiny::selectInput(inputId = 'userRoleSetting_roleName_sel',label = '请选择角色',choices =userRoleSetting_role_query(app_id = app_id) ,selected =userRoleSetting_role_query(app_id = app_id)[1] ,multiple = FALSE,selectize = TRUE)

    })


  })


  shiny::observeEvent(input$userRoleSetting_save_btn,{
     user_name = input$userRoleSetting_userName_sel
     role_name =input$userRoleSetting_roleName_sel
     print(user_name)
     print(role_name)
     userRoleSetting_role_update(app_id = app_id,user_name = user_name,role_name = role_name)
     tsui::run_dataTable2(id = 'userRoleSetting_query_dataview',data = userQuery_query(app_id=app_id,user_name=user_name))




  })








}
