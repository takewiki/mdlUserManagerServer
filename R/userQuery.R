#'用户查询
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param user_name 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userQuery_query()
userQuery_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='') {
  if(user_name == ''){
    sql <- paste0("SELECT
Fuser as  用户名
,fname as 真实姓名
,Fpermissions as 角色
  FROM t_md_userRight")
  }else{
    sql <- paste0("SELECT
Fuser as  用户名
,fname as 真实姓名
,Fpermissions as 角色
  FROM t_md_userRight
  where FappId ='",app_id,"' and  Fuser like'%",user_name,"%'")
  }

  data = tsda::sql_select(conn,sql)

  return(data)


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
#' userQueryServer()
userQueryServer <- function(input,output,session,app_id){
  var_userName = tsui::var_text('userQuery_userName_txt')

  shiny::observeEvent(input$userQuery_query_btn,{
    user_name = var_userName()
    print(user_name)
    if(user_name ==''){
      tsui::pop_notice("请输入用户名!")
    }else{



        data = userQuery_query(app_id = app_id,user_name = user_name)
        tsui::run_dataTable2(id = 'userQuery_query_dataview',data = data)

    }



  })



}
