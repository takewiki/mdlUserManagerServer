#' 检测角色是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 角色名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleAdd_isNew()
roleAdd_isNew <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='standard') {
sql <- paste0(" select  FRoleName  from t_md_role
  where FappId ='",app_id,"' and  FRoleName='",role_name,"'")
data = tsda::sql_select(conn,sql)
ncount =nrow(data)
if(ncount>0){
  res = FALSE
}else{
  res = TRUE
}
return(res)


}


#' 检测角色是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 角色名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleAdd_isNew()
roleAdd_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='standard') {
  sql <- paste0(" select   FRoleName as 角色名称,FNote as 角色备注
 from t_md_role
  where FappId ='",app_id,"' and  FRoleName='",role_name,"'")
  data = tsda::sql_select(conn,sql)

  return(data)


}


#' 检测角色是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 角色名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleAdd_isNew()
roleAdd_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='standard') {
  sql <- paste0(" select   FRoleName as 角色名称,FNote as 角色备注
 from t_md_role
  where FappId ='",app_id,"' and  FRoleName='",role_name,"'")
  data = tsda::sql_select(conn,sql)

  return(data)


}

#' 检测角色最大值
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleAdd_isNew()
roleAdd_MaxId <- function(conn = tsda::conn_rds('rdbe')) {

  data = tsda::db_maxId(conn = conn,FTableName = 't_md_role')

  return(data)


}



#' 检测角色是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 角色名
#'
#' @return 返回值
#' @export
#'
#' @examples
#'
#' roleAdd_objectRight_save()
roleAdd_objectRight_save <- function(conn = tsda::conn_rds('rdbe'),
                         app_id ='cpdms',role_name='demo') {
  sql_del <-paste0("delete from  t_md_objectRight

  where FappId = '",app_id,"' and Fpermissions ='",role_name,"'")
  tsda::sql_update(conn,sql_del)

  sql_ins <- paste0("insert into t_md_objectRight
SELECT  0 as [Fshow]
      ,[Fname]
      ,[Fid]
      ,[Ficon]
      ,'",role_name,"' as [Fpermissions]
      ,[Ftype]
      ,[FparentId]
      ,[Findex]
      ,[FappId]
  FROM [rdbe].[dbo].[t_md_objectRight]
  where FappId = '",app_id,"' and Fpermissions ='standard'")
  tsda::sql_update(conn,sql_ins)




}





#' 检测角色是否存在
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param note 备注
#' @param role_name 角色名
#'
#' @return 返回值
#' @export
#'
#' @examples
#'
#' roleAdd_save()
roleAdd_save <- function(conn = tsda::conn_rds('rdbe'),
                         app_id ='cpdms',role_name='demo',note='') {

  flag = roleAdd_isNew(conn = conn,app_id = app_id ,role_name = role_name)
  if(flag){
    FInterId = roleAdd_MaxId(conn = conn) + 1
    sql <- paste0("  insert into t_md_role
    values(",FInterId,",'",app_id,"','",role_name,"','",note,"',0)")
    tsda::sql_update(conn,sql)
    #同步写入默认对象
    roleAdd_objectRight_save(conn = conn,app_id = app_id,role_name = role_name)
    res = TRUE

  }else{
    res = FALSE
  }
  return(res)

}

#' 角色新增的逻辑处理
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
#' roleAddServer()
roleAddServer <- function(input,output,session,app_id){
  var_roleName = tsui::var_text('roleAdd_roleName_txt')
  var_note = tsui::var_text('roleAdd_note_txt')
  shiny::observeEvent(input$roleAdd_upload_btn,{
    role_name = var_roleName()
    note= var_note()
    print(role_name)
    if(role_name ==''){
      tsui::pop_notice("请输入角色名!")
    }else{
      if(note ==''){
        tsui::pop_notice("请输入角色备注!")

      }else{

        flag = roleAdd_save(app_id = app_id,role_name = role_name,note = note)
        data = roleAdd_query(app_id = app_id,role_name = role_name)
        tsui::run_dataTable2(id = 'roleAdd_query_dataview',data = data)
        if(flag){
          tsui::pop_notice("角色新增成功!")
        }else{
          tsui::pop_notice("角色已经存在!")
        }



      }

    }



  })





}
