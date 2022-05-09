
#' 查询用户
#'
#' @param conn 连接
#' @param app_id 程序ID
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleRight_user_query()
roleRight_role_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms') {
  sql <- paste0("  select FRoleName from t_md_role
  where FappId ='",app_id,"' and Fdeleted =0 and FRoleName not in('admin','standard')")
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
#' @param role_name 角色
#' @param type 类型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleRight_object_query()
roleRight_object_query <- function(conn = tsda::conn_rds('rdbe'),app_id ='cpdms',role_name='财务总监',type='system') {

  if(type =='system'){
    sql <- paste0("  SELECT
  a.Fid as 系统代码
  ,a.Fname as 系统名称
  ,b.fname  as 授权状态
  FROM  t_md_objectRight a
  inner join t_md_logical b
  on a.Fshow = b.fid
  where a.FappId = '",app_id,"' and a.Fpermissions ='",role_name,"' and a.Ftype='",type,"'
  order by Findex")

  }else{

    sql <- paste0("  SELECT
  c.Fid as 系统代码
  ,c.Fname as 系统名称
  ,a.Fid as 功能代码
  ,a.Fname as 功能名称
  ,b.fname as 授权状态
  FROM  t_md_objectRight a
  inner join t_md_logical b
  on a.Fshow = b.fid
  inner join t_md_objectRight c
  on a.FparentId = c.fid  and a.FappId = c.FappId  and a.Fpermissions = c.Fpermissions
  where a.FappId = '",app_id,"' and a.Fpermissions ='",role_name,"' and a.Ftype='",type,"'
  and c.Ftype ='system'
  order by c.Findex,a.Findex")


  }
  cat(sql)
  res = tsda::sql_select(conn,sql)

  return(res)

}





#' 设置权限
#'
#' @param conn 连接
#' @param app_id 程序ID
#' @param role_name 角色
#' @param type 类型
#' @param system_id 系统ID
#' @param module_id  功能ID
#' @param show  是否授权
#'
#' @return 返回值
#' @export
#'
#' @examples
#' roleRight_object_update()
roleRight_object_update <- function(conn = tsda::conn_rds('rdbe'),
                                    app_id ='cpdms',
                                    role_name='财务总监',
                                    type='system',
                                    system_id='A',
                                    module_id='',
                                    show =1) {

  if(type =='system'){
    sql_sys <- paste0("update a set Fshow = ",show,"  from t_md_objectRight a
  where a.FappId = '",app_id,"' and a.Fpermissions ='",role_name,"' and a.Ftype='system'
  and Fid ='",system_id,"'")
    sql_mdl <- paste0("update a set Fshow =",show,"  from t_md_objectRight a
  where a.FappId = '",app_id,"' and a.Fpermissions ='",role_name,"' and a.Ftype='module'
  and FparentId ='",system_id,"'")
    tsda::sql_update(conn,sql_sys)
    tsda::sql_update(conn,sql_mdl)

  }else{

    sql <- paste0("update a set Fshow = ",show,"  from t_md_objectRight a
  where a.FappId = '",app_id,"' and a.Fpermissions ='",role_name,"' and a.Ftype='module'
  and FparentId ='",system_id,"'  and  Fid ='",module_id,"'")
    tsda::sql_update(conn,sql)


  }


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
#' roleRightServer()
roleRightServer <- function(input,output,session,app_id){


  shiny::observeEvent(input$roleRight_getRoleName_btn,{



    output$roleRight_roleName_ph <- shiny::renderUI({
      shiny::selectInput(inputId = 'roleRight_roleName_sel',label = '请选择角色',choices =roleRight_role_query(app_id = app_id) ,selected =roleRight_role_query(app_id = app_id)[1] ,multiple = FALSE,selectize = TRUE)

    })

  })



  #获取对象列表
  var_roleRight_type_lc1 = tsui::var_ListChoose1('roleRight_type_lc1')

  data_object = shiny::eventReactive(input$roleRight_getObject_btn,{
    type = var_roleRight_type_lc1()
    role_name = input$roleRight_roleName_sel
    data = roleRight_object_query(app_id = app_id,role_name = role_name,type = type)
    return(data)
  })


  shiny::observeEvent(input$roleRight_getObject_btn,{

    data = data_object()
    output$roleRight_object_dt <- DT::renderDataTable(data,selection = 'single')




  })

  #设置内容
  object_selected  <- reactive({

    shiny::validate(
      need(length(input$roleRight_object_dt_rows_selected) > 0, "请从对照列表中选中任意一行")
    )
    data_detail <- data_object()
    type = var_roleRight_type_lc1()
    if(type =='system'){
      system_id  <- data_detail[as.integer(input$roleRight_object_dt_rows_selected), '系统代码']
      module_id =''
    }else{
      system_id  <- data_detail[as.integer(input$roleRight_object_dt_rows_selected), '系统代码']
      module_id  <- data_detail[as.integer(input$roleRight_object_dt_rows_selected), '功能代码']
    }

    res = data.frame(type,system_id,module_id)



    return(res)

  })

   var_roleRight_show_lc1 = tsui::var_ListChoose1('roleRight_show_lc1')
  shiny::observeEvent(input$roleRight_done_btn,{
    role_name = input$roleRight_roleName_sel

    flag =input$roleRight_object_dt_rows_selected
    show = as.integer(var_roleRight_show_lc1())
    print(flag)
    if(is.null(flag)){
      tsui::pop_notice("请在右侧列表中选中一行!")
    }else{
      data = object_selected()
      type = data$type
      system_id =data$system_id
      module_id = data$module_id



      # tsui::run_dataTable2('roleRight_query_dataview',data =data )

      roleRight_object_update(app_id = app_id,role_name = role_name,type = type,system_id = system_id,module_id = module_id,show =show )

      tsui::pop_notice(paste0("角色:",role_name,"授权成功!"))
    }




  })










}
