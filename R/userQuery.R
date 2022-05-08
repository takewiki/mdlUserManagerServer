#'用户禁用
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
userQuery_disable <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='') {
  sql <-paste0("update a set a.Fdeleted =1 from t_md_userRight a
  where  FappId ='",app_id,"' and  Fuser='",user_name,"'")
  data = tsda::sql_update(conn,sql)

  return(data)


}
#'用户反禁用
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
userQuery_enable <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='') {
  sql <-paste0("update a set a.Fdeleted =0 from t_md_userRight a
  where  FappId ='",app_id,"' and  Fuser='",user_name,"'")
  data = tsda::sql_update(conn,sql)

  return(data)


}


#'用户查询
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param user_name 用户名
#' @param status 禁用状态
#'
#' @return 返回值
#' @export
#'
#' @examples
#' userQuery_query()
userQuery_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',user_name='',status=-1) {
  if(user_name == ''){
    if(status == -1){
      sql <- paste0("SELECT
a.Fuser as  用户名
,a.fname as 真实姓名
,a.Fpermissions as 角色
,b.fname as  是否禁用
  FROM t_md_userRight  a
  inner join t_md_logical b
  on a.Fdeleted = b.fid  where a.FappId ='",app_id,"'")
    }else{
      sql <- paste0("SELECT
a.Fuser as  用户名
,a.fname as 真实姓名
,a.Fpermissions as 角色
,b.fname as  是否禁用
  FROM t_md_userRight  a
  inner join t_md_logical b
  on a.Fdeleted = b.fid  where a.FappId ='",app_id,"'   and  b.fid =  ",status,"")
    }

  }else{
    if(status == -1){
      sql <- paste0("SELECT
a.Fuser as  用户名
,a.fname as 真实姓名
,a.Fpermissions as 角色
,b.fname as  是否禁用
  FROM t_md_userRight  a
  inner join t_md_logical b
  on a.Fdeleted = b.fid
  where a.FappId ='",app_id,"' and  a.Fuser like'%",user_name,"%'")
    }else{
      sql <- paste0("SELECT
a.Fuser as  用户名
,a.fname as 真实姓名
,a.Fpermissions as 角色
,b.fname as  是否禁用
  FROM t_md_userRight  a
  inner join t_md_logical b
  on a.Fdeleted = b.fid
  where a.FappId ='",app_id,"' and  a.Fuser like'%",user_name,"%'    and  b.fid =  ",status,"")
    }

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
#' userQueryServer_query()
userQueryServer_query <- function(input,output,session,app_id){
  var_userName = tsui::var_text('userQuery_userName_txt')
  var_userQuery_status_lc1 <- tsui::var_ListChoose1('userQuery_status_lc1')


  shiny::observeEvent(input$userQuery_query_btn,{
    user_name = var_userName()
    status = var_userQuery_status_lc1()
    print(status)
    data = userQuery_query(app_id = app_id,user_name = user_name,status = status)
    tsui::run_dataTable2(id = 'userQuery_query_dataview',data = data)

  })



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
#' userQueryServer_query()
userQueryServer_disable <- function(input,output,session,app_id){

  var_userName = tsui::var_text('userQuery_userName_txt')
  shiny::observeEvent(input$userQuery_disable_btn,{
    user_name = var_userName()
    flag = userAdd_isNew(app_id=app_id,user_name=user_name)
    if(flag){
       tsui::pop_notice('请重新选择用户')
    }else{
      userQuery_disable(app_id = app_id,user_name = user_name)
      tsui::pop_notice(paste0("用户:",user_name,"禁用成功!"))
    }


  })


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
#' userQueryServer_query()
userQueryServer_enable <- function(input,output,session,app_id){

  var_userName = tsui::var_text('userQuery_userName_txt')
  shiny::observeEvent(input$userQuery_enable_btn,{
    user_name = var_userName()
    flag = userAdd_isNew(app_id=app_id,user_name=user_name)
    if(flag){
      tsui::pop_notice('请重新选择用户')
    }else{
      userQuery_enable(app_id = app_id,user_name = user_name)
      tsui::pop_notice(paste0("用户:",user_name,"反禁用成功!"))
    }


  })


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
 userQueryServer_query(input,output,session,app_id)
  userQueryServer_disable(input,output,session,app_id)
  userQueryServer_enable(input,output,session,app_id)



}
