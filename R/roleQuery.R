#'用户禁用
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleQuery_query()
roleQuery_disable <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='') {
  sql <-paste0("update a set a.Fdeleted =1 from t_md_role a
  where  FappId ='",app_id,"' and  FRoleName='",role_name,"'")
  data = tsda::sql_update(conn,sql)

  return(data)


}
#'用户反禁用
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 用户名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleQuery_query()
roleQuery_enable <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='') {
  sql <-paste0("update a set a.Fdeleted =0 from t_md_role a
  where  FappId ='",app_id,"' and  FRoleName='",role_name,"'")
  data = tsda::sql_update(conn,sql)

  return(data)


}


#'用户查询
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 用户名
#' @param status 禁用状态
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleQuery_query()
roleQuery_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='',status=-1) {
  if(role_name == ''){
    if(status == -1){
      sql <- paste0("  select

 FRoleName as 角色名称
 ,FNote  as 角色备注
 ,b.fname as 禁用状态
  from t_md_role a
  inner join t_md_logical b
    on a.Fdeleted = b.fid  where a.FappId ='",app_id,"'")
    }else{
      sql <- paste0("select

 FRoleName as 角色名称
 ,FNote  as 角色备注
 ,b.fname as 禁用状态
  from t_md_role a
  inner join t_md_logical b
  on a.Fdeleted = b.fid  where a.FappId ='",app_id,"'   and  b.fid =  ",status,"")
    }

  }else{
    if(status == -1){
      sql <- paste0("select

 FRoleName as 角色名称
 ,FNote  as 角色备注
 ,b.fname as 禁用状态
  from t_md_role a
  inner join t_md_logical b
  on a.Fdeleted = b.fid
  where a.FappId ='",app_id,"' and  a.FRoleName like'%",role_name,"%'")
    }else{
      sql <- paste0("select

 FRoleName as 角色名称
 ,FNote  as 角色备注
 ,b.fname as 禁用状态
  from t_md_role a
  inner join t_md_logical b
  on a.Fdeleted = b.fid
  where a.FappId ='",app_id,"' and  a.FRoleName like'%",role_name,"%'    and  b.fid =  ",status,"")
    }

  }
  print(sql)
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
#' roleQueryServer_query()
roleQueryServer_query <- function(input,output,session,app_id){
  print('bug1')
  var_roleName = tsui::var_text('roleQuery_roleName_txt')
  print('bug2')
  var_roleQuery_status_lc1 <- tsui::var_ListChoose1('roleQuery_status_lc1')
  print('bug3')


  shiny::observeEvent(input$roleQuery_query_btn,{
    print('bug4')
    role_name = var_roleName()
    print('bug5')
    print(role_name)
    status = var_roleQuery_status_lc1()
    print('bug6')
    print(status)
    print('bug7')
    data = roleQuery_query(app_id = app_id,role_name = role_name,status = status)
    print('bug8')
    print(data)
    tsui::run_dataTable2(id = 'roleQuery_query_dataview',data = data)

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
#' roleQueryServer_query()
roleQueryServer_disable <- function(input,output,session,app_id){

  var_roleName = tsui::var_text('roleQuery_roleName_txt')
  shiny::observeEvent(input$roleQuery_disable_btn,{
    role_name = var_roleName()
    flag = roleAdd_isNew(app_id=app_id,role_name=role_name)
    if(flag){
       tsui::pop_notice('请重新选择角色')
    }else{
      roleQuery_disable(app_id = app_id,role_name = role_name)
      tsui::pop_notice(paste0("角色:",role_name,"禁用成功!"))
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
#' roleQueryServer_query()
roleQueryServer_enable <- function(input,output,session,app_id){

  var_roleName = tsui::var_text('roleQuery_roleName_txt')
  shiny::observeEvent(input$roleQuery_enable_btn,{
    role_name = var_roleName()
    flag = roleAdd_isNew(app_id=app_id,role_name=role_name)
    if(flag){
      tsui::pop_notice('请重新选择用户')
    }else{
      roleQuery_enable(app_id = app_id,role_name = role_name)
      tsui::pop_notice(paste0("角色:",role_name,"反禁用成功!"))
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
#' roleQueryServer()
roleQueryServer <- function(input,output,session,app_id){
  print(1)
 roleQueryServer_query(input,output,session,app_id)
 print(2)
  roleQueryServer_disable(input,output,session,app_id)
  print(3)
  roleQueryServer_enable(input,output,session,app_id)
  print(4)



}
