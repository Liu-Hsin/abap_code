report z_ave. " AVE - Abap Versions Explorer/Code Reviewer
interface zif_ave_popup_types deferred.
interface zif_ave_object deferred.
interface zif_ave_acr_types deferred.
class zcl_ave_vrsd definition deferred.
class zcl_ave_versno definition deferred.
class zcl_ave_version definition deferred.
class zcl_ave_request definition deferred.
class zcl_ave_progress definition deferred.
class zcl_ave_popup_html definition deferred.
class zcl_ave_popup_diff definition deferred.
class zcl_ave_popup_data definition deferred.
class zcl_ave_popup definition deferred.
class zcl_ave_object_tr definition deferred.
class zcl_ave_object_prog definition deferred.
class zcl_ave_object_pack definition deferred.
class zcl_ave_object_intf definition deferred.
class zcl_ave_object_func definition deferred.
class zcl_ave_object_factory definition deferred.
class zcl_ave_object_ddls definition deferred.
class zcl_ave_object_clas definition deferred.
class zcl_ave_author definition deferred.
class zcl_ave_acr_stats definition deferred.
class zcl_ave_acr_report definition deferred.
class zcl_ave_acr_note_dlg definition deferred.
"! Exception class for AVE (Abap Versions Explorer)
class zcx_ave definition
  inheriting from cx_static_check
  create public.

  public section.

    methods constructor
      importing
        !textid   like if_t100_message=>t100key optional
        !previous like previous optional.

    class-methods raise_from_syst
      raising
        zcx_ave.

endclass.
class zcx_ave implementation.

  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.
  endmethod.

  method raise_from_syst.
    try.
        cx_proxy_t100=>raise_from_sy_msg( ).
      catch cx_proxy_t100 into data(exc_t100).
        raise exception type zcx_ave
          exporting
            previous = exc_t100.
    endtry.
  endmethod.

endclass.

interface zif_ave_acr_types .

  types ty_approved type hashed table of string with unique key table_line.

  "! Per-author change contribution inside one object diff
  types:
    begin of ty_author_stats,
      author      type versuser,
      author_name type ad_namtext,
      ins_count   type i,
      del_count   type i,
      mod_count   type i,
      hunk_count  type i,
    end of ty_author_stats.
  types ty_t_author_stats type standard table of ty_author_stats with default key.

  "! Per-reviewer action totals for the report header
  types:
    begin of ty_reviewer_stats,
      reviewer      type syuname,
      reviewer_name type ad_namtext,
      appr_count    type i,
      decl_count    type i,
      total_count   type i,
      saved_at      type timestampl,
    end of ty_reviewer_stats.
  types ty_t_reviewer_stats type standard table of ty_reviewer_stats with default key.

  "! Statistics for one changed object: version pair, counts, blame breakdown
  types:
    begin of ty_obj_stats,
      objtype      type versobjtyp,
      class_name   type seoclsname,   " parent class for METH / CPUB / CPRO / CPRI / CINC
      obj_name     type versobjnam,
      versno_new   type versno,
      versno_old   type versno,
      author       type versuser,
      author_name  type ad_namtext,
      datum        type versdate,
      zeit         type verstime,
      ins_count    type i,
      del_count    type i,
      mod_count    type i,
      hunk_count   type i,
      display_name type string,
      bt_authors   type ty_t_author_stats,
      is_created   type abap_bool,   " abap_true = object is brand-new (no prior version)
    end of ty_obj_stats.
  types ty_t_obj_stats type standard table of ty_obj_stats with default key.
endinterface.

interface zif_ave_object.

  "! Popup display settings (maps to selection screen checkboxes)
  types:
    begin of ty_settings,
      show_diff   type abap_bool,
      layout      type abap_bool,
      two_pane    type abap_bool,
      no_toc      type abap_bool,
      ignore_case type abap_bool,
      compact     type abap_bool,
      remove_dup  type abap_bool,
      blame       type abap_bool,
      filter_user type versuser,
      date_from   type versdate,
      code_review type abap_bool,
    end of ty_settings.

  "! A single versionable part of an object (e.g. one method, one include)
  types:
    begin of ty_part,
      class       type string,      "class
      unit        type string,      "method/include
      object_name type versobjnam,   " VRSD object name
      type        type versobjtyp,   " VRSD object type (REPS, METH, CLSD, …)
    end of ty_part,
    ty_t_part type standard table of ty_part with default key.

  "! Returns the list of versionable parts for this object
  methods get_parts
    returning
      value(result) type ty_t_part
    raising
      zcx_ave.

  "! Returns the logical object name
  methods get_name
    returning
      value(result) type string.

  "! Returns TRUE if the object exists in the current system
  methods check_exists
    returning
      value(result) type abap_bool.

endinterface.

interface zif_ave_popup_types.

  "! One diff operation: op = '=' (equal), '-' (deleted), '+' (inserted)
  types:
    begin of ty_diff_op,
      op(1) type c,
      text  type string,
    end of ty_diff_op.
  types ty_t_diff type standard table of ty_diff_op with default key.

  "! Version row: one VRSD entry enriched with author/task/request display data.
  types:
    begin of ty_version_row,
      objname        type versobjnam,
      versno         type versno,
      versno_text    type string,
      datum          type versdate,
      zeit           type verstime,
      author         type versuser,
      author_name    type ad_namtext,
      obj_owner      type versuser,
      obj_owner_name type ad_namtext,
      korrnum        type verskorrno,
      task           type trkorr,
      korr_text      type string,
      objtype        type versobjtyp,
      trfunction     type e070-trfunction,
      rowcolor(4)    type c,
    end of ty_version_row.
  types ty_t_version_row type standard table of ty_version_row with default key.

  "! Blame entry: a source line annotated with author/version info
  types:
    begin of ty_blame_entry,
      text        type string,
      author      type versuser,
      author_name type ad_namtext,
      datum       type versdate,
      zeit        type verstime,
      versno_text type string,
      korrnum     type verskorrno,
      task        type trkorr,
      task_text   type string,
    end of ty_blame_entry.
  types ty_blame_map type standard table of ty_blame_entry with default key.

endinterface.

class zcl_ave_acr_note_dlg definition
  final
  create public.

  public section.
    "! Opens a non-blocking text-editor dialog for entering a Decline note.
    "! Logic: close with text → SAVED event raised (decline registered).
    "!        close with empty text → nothing happens (decline cancelled).
    "! iv_title    : dialog caption, e.g. "METH~MY_METHOD - Block 3"
    "! iv_hunk_key : opaque key passed back unchanged in the SAVED event
    "! iv_note     : pre-filled text (for Edit Review)
    events saved
      exporting
        value(iv_hunk_key) type string
        value(iv_note)     type string.
    events cancelled
      exporting
        value(iv_hunk_key) type string.

    methods constructor
      importing iv_title    type string
                iv_hunk_key type string
                iv_note     type string optional.

    methods show.

  private section.
    data mv_title    type string.
    data mv_hunk_key type string.
    data mv_note     type string.

    data mo_box      type ref to cl_gui_dialogbox_container.
    data mo_text     type ref to cl_gui_textedit.

    methods on_box_close
      for event close of cl_gui_dialogbox_container
      importing sender.
endclass.
class zcl_ave_acr_report definition
  final
  create private.

  public section.
    "! Build the Code Review Report HTML page from pre-computed object stats.
    class-methods to_html
      importing it_obj_stats  type zif_ave_acr_types=>ty_t_obj_stats
                i_korrnum     type trkorr
                it_approved   type zif_ave_acr_types=>ty_approved optional
                it_declined   type zif_ave_acr_types=>ty_approved optional
                it_reviewers  type zif_ave_acr_types=>ty_t_reviewer_stats optional
      returning value(result) type string.

  protected section.
  private section.
    class-methods esc
      importing iv_val        type clike
      returning value(result) type string.

endclass.
class zcl_ave_acr_stats definition
  final
  create private.

  public section.
    "! Compute ins/del/mod counts from a diff, mirroring the pairing logic of diff_to_html.
    "! When it_blame is supplied, also builds per-author contribution in et_authors
    "! including per-author hunk_count (each change block attributed to the first blamed line).
    "! Hunks consisting entirely of blank/whitespace lines are excluded from hunk_count.
    class-methods from_diff
      importing it_diff    type zif_ave_popup_types=>ty_t_diff
                it_blame   type zif_ave_popup_types=>ty_blame_map optional
      exporting ev_ins     type i
                ev_del     type i
                ev_mod     type i
                et_authors type zif_ave_acr_types=>ty_t_author_stats.

    "! Returns abap_true if every changed line in the hunk is blank/whitespace-only.
    class-methods is_blank_hunk
      importing it_lines      type string_table
      returning value(result) type abap_bool.

  protected section.
  private section.
    class-methods add_blame
      importing iv_text     type string
                iv_op       type c            " '+' = ins, '~' = mod
                iv_new_hunk type abap_bool default abap_false
                it_blame    type zif_ave_popup_types=>ty_blame_map
      changing  ct_authors  type zif_ave_acr_types=>ty_t_author_stats.

endclass.
"! Resolves SAP username to display name, with caching
class zcl_ave_author definition
  final
  create public.

  public section.

    "! Returns the user's full name, or the username if the user no longer exists
    methods get_name
      importing
        !uname        type syuname
      returning
        value(result) type string.

  protected section.
  private section.

    types:
      begin of ty_s_author,
        uname type syuname,
        name  type string,
      end of ty_s_author,
      ty_t_author type sorted table of ty_s_author with unique key uname.

    class-data authors type ty_t_author.

endclass.
"! Object handler for an ABAP class.
"! Returns class sections (pool, pub/pro/pri, local types/impl) plus all methods.
class zcl_ave_object_clas definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !name type seoclsname
      raising
        zcx_ave.

  protected section.
  private section.
    data name type seoclsname.

endclass.
"! Object handler for a CDS View (DDLS).
"! Returns one part of type DDLS; source is loaded via cl_svrs_tlogo_controller.
class zcl_ave_object_ddls definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !name type versobjnam.

  private section.
    data name type versobjnam.

endclass.
"! Factory for AVE object handlers. Creates the right handler by object type string.
class zcl_ave_object_factory definition
  final
  create public.

  public section.

    constants:
      begin of gc_type,
        program  type string value 'PROG',
        class    type string value 'CLAS',
        intf     type string value 'INTF',
        function type string value 'FUNC',
        tr       type string value 'TR',
        package  type string value 'DEVC',
        ddls     type string value 'DDLS',
      end of gc_type.

    "! Returns an object handler for the given type+name.
    "! Raises ZCX_AVE if the object does not exist.
    methods get_instance
      importing
        object_type   type string
        object_name   type sobj_name
      returning
        value(result) type ref to zif_ave_object
      raising
        zcx_ave.

endclass.
"! Object handler for a function module (single FUNC part)
class zcl_ave_object_func definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !name type rs38l_fnam.

  private section.
    data name type rs38l_fnam.

endclass.
"! Object handler for an ABAP interface (one INTF part)
class zcl_ave_object_intf definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !name type seoclsname.

  private section.
    data name type seoclsname.

endclass.
"! Object handler for a Development Package (DEVCLASS).
"! Reads all objects from TADIR and delegates to specific object handlers.
class zcl_ave_object_pack definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !id type devclass.

  private section.

    data id type devclass.

    types ty_t_object type table of ref to zif_ave_object with key table_line.

    methods get_object_keys
      returning
        value(result) type trwbo_t_e071
      raising
        zcx_ave.

    methods get_object
      importing
        object_key    type trwbo_s_e071
      returning
        value(result) type ref to zif_ave_object.

endclass.
"! Object handler for a single program or include (one REPS part)
class zcl_ave_object_prog definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !name type sobj_name.

  private section.
    data name type sobj_name.

endclass.
"! Object handler for a Transport Request or Task.
"! Reads all objects from the TR and delegates to specific object handlers.
class zcl_ave_object_tr definition
  final
  create public.

  public section.

    interfaces zif_ave_object.

    methods constructor
      importing
        !id type trkorr.

  protected section.
  private section.

    data id type trkorr.

    types ty_t_object type table of ref to zif_ave_object with key table_line.

    methods get_object_keys
      returning
        value(result) type trwbo_t_e071
      raising
        zcx_ave.

    methods get_objects_for_keys
      importing
        object_keys   type trwbo_t_e071
      returning
        value(result) type ty_t_object.

    methods get_object
      importing
        object_key    type trwbo_s_e071
      returning
        value(result) type ref to zif_ave_object.

endclass.
class zcl_ave_popup definition
  final
  create public.

  public section.

    methods constructor
      importing
        i_object_type type string
        i_object_name type string
        is_settings   type zif_ave_object=>ty_settings optional.

    methods show.

  protected section.
  private section.

    types:
      "──────────── types ─────────────────────────────────────────────
      " Extended parts row: original fields + existence flag + row color
      begin of ty_part_row,
        class       type string,
        name        type string,
        type        type versobjtyp,
        type_text   type as4text,
        object_name type versobjnam,
        exists_flag type abap_bool,
        rows        type i,
        rowcolor(4) type c,
      end of ty_part_row .
    types:
    ty_t_part_row type standard table of ty_part_row with default key .
    types ty_version_row type zif_ave_popup_types=>ty_version_row .
    types ty_t_version_row type zif_ave_popup_types=>ty_t_version_row .
    "! Delegated to ZCL_AVE_POPUP_DIFF (extracted diff engine)
    types ty_diff_op type zif_ave_popup_types=>ty_diff_op .
    types ty_t_diff type zif_ave_popup_types=>ty_t_diff .
    "! Delegated to ZCL_AVE_POPUP_HTML (extracted HTML renderer)
    types ty_blame_entry type zif_ave_popup_types=>ty_blame_entry .
    types ty_blame_map type zif_ave_popup_types=>ty_blame_map .
    "──────────── diff HTML cache ────────────────────────────────────
    "! Per-instance cache for rendered diff HTML.
    "! Key: object type/name + old/new versno + display flags (blame/two_pane/compact/debug).
    "! Hit: return stored HTML immediately, skipping source load, diff and blame computation.
    "! Miss: compute as usual, store result. Cache lives for the lifetime of the popup instance.
    types: begin of ty_diff_cache_key,
             objtype     type versobjtyp,
             objname     type versobjnam,
             versno_o    type versno,
             versno_n    type versno,
             blame       type abap_bool,
             two_pane    type abap_bool,
             compact     type abap_bool,
             debug       type abap_bool,
             ignore_case type abap_bool,
           end of ty_diff_cache_key.
    types: begin of ty_diff_cache,
             key  type ty_diff_cache_key,
             html type string,
           end of ty_diff_cache.
    types ty_t_diff_cache type hashed table of ty_diff_cache with unique key key.

    "──────────── controls ──────────────────────────────────────────
    class-data mv_counter type i .
    data mv_object_type type string .
    data mv_object_name type string .
    data mo_box type ref to cl_gui_dialogbox_container .
    data mo_split_main type ref to cl_gui_splitter_container .
    data mo_split_top type ref to cl_gui_splitter_container .
    data mo_cont_parts type ref to cl_gui_container .
    data mo_cont_html type ref to cl_gui_container .
    data mo_cont_vers type ref to cl_gui_container .
    " 2-pane layout containers
    data mo_split_wrap type ref to cl_gui_splitter_container .
    data mo_split_2p_top type ref to cl_gui_splitter_container .
    data mo_split_2p_wrap type ref to cl_gui_splitter_container .
    data mv_focus_html type abap_bool value abap_false ##NO_TEXT.
    data mo_cont_parts_2p type ref to cl_gui_container .
    data mo_cont_vers_2p type ref to cl_gui_container .
    data mo_cont_html_2p type ref to cl_gui_container .
    " Left panel: ALV Grid with the list of object parts
    data mo_alv_parts type ref to cl_gui_alv_grid .
    data mt_parts type ty_t_part_row .
    " Right panel: HTML code viewer + ABAP editor (used for single-version
    " source view; HTML is too slow for 100k+ lines)
    data mo_html type ref to cl_gui_html_viewer .
    data mo_code_viewer type ref to cl_gui_abapedit .
    " Splits mo_cont_html into two rows — HTML (diff) on top, ABAP editor
    " (single-version source) on bottom. We toggle row heights 0/100 to
    " switch views reliably (z-order tricks with set_visible are unreliable).
    data mo_split_html type ref to cl_gui_splitter_container .
    data mo_cont_html_diff type ref to cl_gui_container .
    data mo_cont_html_code type ref to cl_gui_container .
    " Bottom panel: SALV table with version list
    data mo_alv_vers type ref to cl_gui_alv_grid .
    data mt_versions type ty_t_version_row .
    data mv_cur_objtype type versobjtyp .
    data mv_cur_objname type versobjnam .
    data mv_cur_part_name type string .  " Human-readable display name for caption (e.g. method name, section name)
    data mv_cur_creator type versuser .
    data ms_base_ver type ty_version_row .
    data ms_diff_old type ty_version_row .
    data ms_diff_new type ty_version_row .
    data mv_show_diff type abap_bool value abap_true ##NO_TEXT.
    data mv_layout type abap_bool .
    data mv_two_pane type abap_bool value abap_true ##NO_TEXT.
    data mv_no_toc type abap_bool value abap_true ##NO_TEXT.
    data mv_compact type abap_bool value abap_true ##NO_TEXT.
    data mv_remove_dup type abap_bool value abap_false ##NO_TEXT.
    data mv_blame type abap_bool value abap_false ##NO_TEXT.
    data mv_ignore_case type abap_bool value abap_true ##NO_TEXT.
    data mv_task_view type abap_bool value abap_false ##NO_TEXT.
    data mv_diff_prev type abap_bool value abap_true ##NO_TEXT.
    data mv_refreshing type abap_bool value abap_false ##NO_TEXT.
    data mv_debug type abap_bool value abap_false ##NO_TEXT.
    data mv_last_html type string .
    "! When drilled into a class from a TR parts view, holds the class name so
    "! Refresh reloads only that class (not the outer TR).
    data mv_drilled_class type seoclsname .
    data mv_filter_user type versuser .
    data mv_date_from type versdate .
    data mv_viewed_versno type versno .
    " Backup for Back navigation (one level)
    data mt_parts_backup type ty_t_part_row .
    data mt_diff_cache type ty_t_diff_cache .
    data mo_toolbar type ref to cl_gui_toolbar .
    data mo_cont_toolbar type ref to cl_gui_container .
    " ── Code Reviewer mode ──────────────────────────────────────────
    data mv_code_review      type abap_bool value abap_false ##NO_TEXT.
    data mv_cr_prepared      type abap_bool value abap_false ##NO_TEXT.
    data mt_acr_stats        type zif_ave_acr_types=>ty_t_obj_stats.
    data mv_cr_report_html   type string.
    data mt_approved         type zif_ave_acr_types=>ty_approved.
    data mt_declined         type zif_ave_acr_types=>ty_approved.
    types ty_action_code type c length 1.
    types: begin of ty_hunk_action,
             hunk_key      type string,
             reviewer      type syuname,
             reviewer_name type ad_namtext,
             action        type ty_action_code,
             changed_at    type timestampl,
           end of ty_hunk_action.
    types ty_t_hunk_actions type standard table of ty_hunk_action with default key.
    " Decline notes: key = hunk key (OBJTYPE~OBJNAME~N), value = note text
    types: begin of ty_decline_note,
             hunk_key type string,
             note     type string,
           end of ty_decline_note.
    types ty_t_decline_notes type hashed table of ty_decline_note with unique key hunk_key.
    types: begin of ty_decline_msg,
             author      type syuname,
             author_name type ad_namtext,
             created_at  type timestampl,
             is_decline  type abap_bool,
             text        type string,
           end of ty_decline_msg.
    types ty_t_decline_msgs type standard table of ty_decline_msg with default key.
    types: begin of ty_hunk_info,
             hunk_key     type string,
             objtype      type versobjtyp,
             obj_name     type versobjnam,
             class_name   type seoclsname,
             display_name type string,
             hunk_no      type i,
             start_line   type i,
             change_count type i,
             change_kind  type string,
             author       type versuser,
             author_name  type ad_namtext,
             html         type string,
           end of ty_hunk_info.
    types ty_t_hunk_info type hashed table of ty_hunk_info with unique key hunk_key.
    types: begin of ty_hunk_thread,
             hunk_key     type string,
             objtype      type versobjtyp,
             obj_name     type versobjnam,
             class_name   type seoclsname,
             display_name type string,
             hunk_no      type i,
             start_line   type i,
             change_count type i,
             change_kind  type string,
             html         type string,
             messages     type ty_t_decline_msgs,
           end of ty_hunk_thread.
    types ty_t_hunk_threads type hashed table of ty_hunk_thread with unique key hunk_key.
    types: begin of ty_saved_thread,
             hunk_key     type string,
             objtype      type versobjtyp,
             obj_name     type versobjnam,
             class_name   type seoclsname,
             display_name type string,
             hunk_no      type i,
             start_line   type i,
             change_count type i,
             change_kind  type string,
             author       type versuser,
             author_name  type ad_namtext,
             html         type string,
             messages     type ty_t_decline_msgs,
           end of ty_saved_thread.
    types ty_t_saved_threads type standard table of ty_saved_thread with default key.
    types: begin of ty_saved_key,
             hunk_key type string,
           end of ty_saved_key.
    types ty_t_saved_keys type standard table of ty_saved_key with default key.
    types: begin of ty_saved_note,
             hunk_key type string,
             note     type string,
           end of ty_saved_note.
    types ty_t_saved_notes type standard table of ty_saved_note with default key.
    types: begin of ty_saved_user_state,
             reviewer      type syuname,
             reviewer_name type ad_namtext,
             saved_at      type timestampl,
             approved      type ty_t_saved_keys,
             declined      type ty_t_saved_keys,
             notes         type ty_t_saved_notes,
           end of ty_saved_user_state.
    types ty_t_saved_user_state type standard table of ty_saved_user_state with default key.
    types: begin of ty_saved_history,
             saved_at       type timestampl,
             saved_by       type syuname,
             saved_by_name  type ad_namtext,
             approved_count type i,
             declined_count type i,
             note_count     type i,
           end of ty_saved_history.
    types ty_t_saved_history type standard table of ty_saved_history with default key.
    types: begin of ty_saved_payload,
             schema_version type i,
             trkorr         type trkorr,
             last_saved_at  type timestampl,
             last_saved_by  type syuname,
             obj_stats      type zif_ave_acr_types=>ty_t_obj_stats,
             hunks          type ty_t_hunk_info,
             diff_cache     type ty_t_diff_cache,
             hunk_actions   type ty_t_hunk_actions,
             user_states    type ty_t_saved_user_state,
             threads        type ty_t_saved_threads,
             history        type ty_t_saved_history,
           end of ty_saved_payload.
    data mt_decline_notes    type ty_t_decline_notes.
    data mt_hunk_actions     type ty_t_hunk_actions.
    data mt_hunk_info        type ty_t_hunk_info.
    data mt_hunk_threads     type ty_t_hunk_threads.
    data mv_cr_base_html     type string.
    data mv_cr_cur_key       type string.
    data mv_cr_report_scroll type i.
    data mv_decline_view_user type versuser.
    data mv_reviewer_view     type abap_bool.
    " Pending decline key — set before opening note dialog, used in saved-event handler
    data mv_pending_decline  type string.
    data mv_pending_edit     type string.
    data mo_note_dlg         type ref to zcl_ave_acr_note_dlg.
    data mo_help_box         type ref to cl_gui_dialogbox_container.
    data mo_help_html        type ref to cl_gui_html_viewer.

    "──────────── build ─────────────────────────────────────────────
    methods build_layout .
    methods build_parts_list .
    methods build_html_viewer .
    methods refresh_vers .
    methods refresh_parts .
    methods switch_pane_layout .
    methods create_parts_alv .
    methods create_versions_alv .
    methods create_html_viewer .
    methods build_versions_grid .
    "──────────── events ────────────────────────────────────────────
    methods handle_parts_toolbar
      for event toolbar of cl_gui_alv_grid
      importing
        !e_object
        !e_interactive .
    methods handle_parts_command
      for event user_command of cl_gui_alv_grid
      importing
        !e_ucomm .
    methods handle_parts_dblclick
      for event double_click of cl_gui_alv_grid
      importing
        !es_row_no
        !e_column .
    methods on_toolbar_click
      for event function_selected of cl_gui_toolbar
      importing
        !fcode .
    methods handle_vers_toolbar
      for event toolbar of cl_gui_alv_grid
      importing
        !e_object
        !e_interactive .
    methods handle_vers_command
      for event user_command of cl_gui_alv_grid
      importing
        !e_ucomm .
    methods handle_vers_dblclick
      for event double_click of cl_gui_alv_grid
      importing
        !es_row_no
        !e_column .
    methods on_box_close
      for event close of cl_gui_dialogbox_container
      importing
        !sender .
    methods on_help_box_close
      for event close of cl_gui_dialogbox_container
      importing
        !sender .
    methods on_sapevent
      for event sapevent of cl_gui_html_viewer
      importing
        !action
        !getdata
        !postdata .
    methods inject_approve_btn
      importing
        !iv_html      type string
        !iv_key       type string
      returning
        value(result) type string .
    methods acr_approve_cell
      importing
        !iv_key       type string
      returning
        value(result) type string .
    methods acr_approve_fixed
      importing
        !iv_key       type string
      returning
        value(result) type string .
    methods refresh_rpt_row .
    methods regen_acr_report .
    methods build_cr_object_report_html
      returning
        value(result) type string .
    methods prepare_code_review
      importing
        !iv_keys type string optional .
    methods delete_and_recalc_selected
      importing
        !iv_keys type string .
    methods show_recalc_picker .
    methods open_saved_code_review
      returning
        value(result) type abap_bool .
    methods maximize_html .
    methods on_note_dlg_saved
      for event saved of zcl_ave_acr_note_dlg
      importing
        !iv_hunk_key
        !iv_note .
    methods on_note_dlg_cancelled
      for event cancelled of zcl_ave_acr_note_dlg
      importing
        !iv_hunk_key .
    methods back_to_report .
    methods show_user_declines
      importing
        !iv_user     type versuser
        !iv_reviewer type abap_bool optional .
    methods open_cr_part
      importing
        !iv_objtype type versobjtyp
        !iv_objname type versobjnam .
    methods rerender_cr_current
      returning
        value(result) type abap_bool .
    methods rerender_cr_user_view
      returning
        value(result) type abap_bool .
    "──────────── logic ─────────────────────────────────────────────
    methods get_class_parts
      importing
        !i_name       type versobjnam
      returning
        value(result) type ty_t_part_row
      raising
        zcx_ave .
    methods load_versions
      importing
        !i_objtype type versobjtyp
        !i_objname type versobjnam .
    methods load_versions_task_view
      importing
        !i_objtype type versobjtyp
        !i_objname type versobjnam .
    methods update_ver_colors
      importing
        !iv_viewed_versno type versno optional .
    methods show_source
      importing
        !i_objtype type versobjtyp
        !i_objname type versobjnam
        !i_versno  type versno .
    methods show_versions_diff
      importing
        !is_old type ty_version_row
        !is_new type ty_version_row .
    "! Auto-open guard: if is_new source exceeds 1000 lines, show source only;
    "! user can manually trigger a diff from the version list.
    methods auto_show_diff_or_source
      importing
        !is_old type ty_version_row
        !is_new type ty_version_row .
    methods set_html
      importing
        !iv_html type string .
    methods has_review_table
      returning
        value(result) type abap_bool .
    methods load_review_from_db .
    methods load_review_payload
      importing
        !iv_trkorr    type trkorr
      exporting
        !es_payload   type ty_saved_payload
      returning
        value(result) type abap_bool .
    methods save_review_to_db
      importing
        !iv_silent type abap_bool optional .
    methods render_decline_thread_html
      importing
        !iv_hunk_key  type string
      returning
        value(result) type string .
    methods render_hunk_actions_html
      importing
        !iv_hunk_key  type string
      returning
        value(result) type string .
    methods render_comment_links
      importing
        !iv_hunk_key  type string
      returning
        value(result) type string .
    methods get_last_own_comment
      importing
        !iv_hunk_key  type string
      returning
        value(result) type string .
    methods format_timestamp
      importing
        !iv_timestamp type timestampl
      returning
        value(result) type string .
    methods set_hunk_action
      importing
        !iv_hunk_key type string
        !iv_action   type ty_action_code .
    methods clear_hunk_action
      importing
        !iv_hunk_key type string .
    methods render_hunk_action_meta
      importing
        !iv_hunk_key  type string
        !iv_action    type ty_action_code
      returning
        value(result) type string .
    methods get_hunk_global_action
      importing
        !iv_hunk_key  type string
      returning
        value(result) type ty_action_code .
    methods sanitize_review_state .
    methods collect_report_status
      exporting
        !et_approved type zif_ave_acr_types=>ty_approved
        !et_declined type zif_ave_acr_types=>ty_approved .
    methods is_own_hunk
      importing
        !iv_hunk_key  type string
      returning
        value(result) type abap_bool .
    methods get_reviewer_stats
      returning
        value(result) type zif_ave_acr_types=>ty_t_reviewer_stats .
    methods build_review_help_html
      returning
        value(result) type string .
    methods show_review_help_popup .
    "! Upload source to the ABAP editor and toggle visibility so it takes the
    "! place of the HTML viewer. Used for single-version (Show Vers) view.
    methods show_code_source
      importing
        !it_source type abaptxt255_tab .
    "! Code Reviewer: compute diff+HTML+stats for one changed part and cache them.
    "! Mirrors the core of show_versions_diff but without UI side effects.
    methods cr_precompute_part
      importing
        !is_part type ty_part_row .
    "! Code Reviewer: iterate all parts of a class, call cr_precompute_part for each.
    "! Returns true if at least one part was added to mt_acr_stats.
    methods cr_precompute_class_parts
      importing
        !i_class_name type seoclsname
      returning
        value(result) type abap_bool .
endclass.
class zcl_ave_popup_data definition
  final
  create private.

  public section.
    class-data mv_no_toc type abap_bool.

    "! Full name of a user (USR01/AD display name).
    class-methods get_user_name
      importing iv_user       type versuser
      returning value(result) type ad_namtext.

    "! Author of the most recent version of an object (from VRSD).
    class-methods get_latest_author
      importing i_type        type versobjtyp
                i_name        type versobjnam
      returning value(result) type versuser.

    "! True if the object exists in the system (TADIR / SEOCOMPO check).
    class-methods check_part_exists
      importing i_type        type versobjtyp
                i_name        type versobjnam
                i_class_name  type seoclsname optional
      returning value(result) type abap_bool.

    "! Object-type description text (lazy-loaded from TRINT_OBJECT_TABLE, cached).
    class-methods get_type_text
      importing i_type        type versobjtyp
      returning value(result) type as4text.

    "! True if any part of the class has changed vs its prior K-type version.
    class-methods check_class_has_author
      importing i_class_name  type string
                i_korrnum     type verskorrno optional
      returning value(result) type abap_bool.

    "! True if the latest version of the object was authored by i_user AND
    "! its source differs from the nearest prior version whose transport
    "! Builds a version list (newest-first, trfunction filled) for a given object.
    "! Used to feed is_substantive_user_change without extra DB queries at check time.
    class-methods build_versions_for_check
      importing i_type        type versobjtyp
                i_name        type versobjnam
      returning value(result) type zif_ave_popup_types=>ty_t_version_row.

    "! Returns true if the latest version in it_versions differs from the
    "! nearest prior K-type version (source comparison).
    class-methods is_substantive_user_change
      importing it_versions   type zif_ave_popup_types=>ty_t_version_row
                i_type        type versobjtyp
                i_name        type versobjnam
                i_korrnum     type verskorrno optional
      returning value(result) type abap_bool.

    "! Drop consecutive versions whose source is identical (ignoring leading
    "! whitespace unless i_ignore_case is false). Input must be sorted newest-first.
    "! i_keep_korrnum: version with this korrnum is never removed (e.g. current TR baseline).
    "! When filled, source comparison is limited to the relevant window around this TR.
    class-methods remove_duplicate_versions
      importing i_keep_korrnum type trkorr optional
                i_ignore_case  type abap_bool default abap_true
      changing  ct_versions    type zif_ave_popup_types=>ty_t_version_row.

    "! Line count of the currently active source for a part (0 when unavailable,
    "! e.g. for CLSD/RELE which have no source).
    class-methods get_active_line_count
      importing i_type        type versobjtyp
                i_name        type versobjnam
      returning value(result) type i.

    "! Read source of a single version. Builds a synthetic VRSD row if none
    "! is stored yet (e.g. version pending in an unreleased task).
    class-methods get_ver_source
      importing i_objtype     type versobjtyp
                i_objname     type versobjnam
                i_versno      type versno
                i_korrnum     type trkorr  optional
                i_author      type versuser optional
                i_datum       type versdate optional
                i_zeit        type verstime optional
      returning value(result) type abaptxt255_tab.

  protected section.
  private section.
    types:
      begin of ty_type_text,
        type type versobjtyp,
        text type as4text,
      end of ty_type_text.
    class-data mt_type_cache type hashed table of ty_type_text with unique key type.
    class-data mv_cache_loaded type abap_bool value abap_false.
    class-methods load_type_cache.
endclass.
class zcl_ave_popup_diff definition
  final
  create private.

  public section.
    "! Type aliases from ZIF_AVE_POPUP_TYPES (defined there for standalone compatibility)
    types ty_diff_op type zif_ave_popup_types=>ty_diff_op.
    types ty_t_diff  type zif_ave_popup_types=>ty_t_diff.

    "! Line-level LCS diff between two source tables.
    class-methods compute_diff
      importing it_old        type abaptxt255_tab
                it_new        type abaptxt255_tab
                i_title       type csequence default 'Computing diff'
                i_confirm_key type csequence optional
                i_ignore_case type abap_bool default abap_false
      returning value(result) type ty_t_diff.

    "! Inline char-level diff for a single line pair.
    "!   iv_side = 'B' → both sides inline (default)
    "!   iv_side = 'N' → only insertion highlighted (new side)
    "!   iv_side = 'O' → only deletion highlighted (old side)
    class-methods char_diff_html
      importing iv_old         type string
                iv_new         type string
                iv_side        type c default 'B'
                iv_ignore_case type abap_bool default abap_false
      returning value(result)  type string.

    "! True if iv_a and iv_b are similar enough for pairing in change blocks.
    "! Used by diff_to_html to decide whether two changed lines are similar enough to pair.
    class-methods has_common_chars
      importing iv_a          type string
                iv_b          type string
      returning value(result) type abap_bool.

    "! Count edit runs in the middle parts of two strings (after stripping common prefix/suffix).
    "! Tokenizes by spaces and does a greedy forward LCS on tokens.
    "! Public so debug_diff_html can display per-pair metrics.
    class-methods count_edit_runs
      importing iv_a          type string
                iv_b          type string
      returning value(result) type i.

    "! Build a blame map by replaying diffs between consecutive versions in
    "! [i_from, i_to] for (i_objtype, i_objname). For every '+' line the current
    "! version's author is recorded; '-' lines go to et_blame_deleted.
    class-methods build_blame_map
      importing it_versions      type zif_ave_popup_types=>ty_t_version_row
                i_objtype        type versobjtyp
                i_objname        type versobjnam
                i_from           type versno
                i_to             type versno
      exporting et_blame_deleted type zif_ave_popup_types=>ty_blame_map
      returning value(result)    type zif_ave_popup_types=>ty_blame_map.

  protected section.
  private section.
    class-methods collapse_token_ops
      changing ct_ops type ty_t_diff.
endclass.
class zcl_ave_popup_html definition
  final
  create private.

  public section.
    "! Type aliases from ZIF_AVE_POPUP_TYPES (defined there for standalone compatibility)
    types ty_blame_entry type zif_ave_popup_types=>ty_blame_entry.
    types ty_blame_map   type zif_ave_popup_types=>ty_blame_map.

    "! Format a source table as a stand-alone HTML page with line numbers.
    class-methods source_to_html
      importing it_source      type abaptxt255_tab
                i_title        type string
                i_meta         type string optional
      returning value(rv_html) type string.

    "! Render a diff (from ZCL_AVE_POPUP_DIFF) as an HTML page.
    class-methods diff_to_html
      importing it_diff          type zif_ave_popup_types=>ty_t_diff
                i_title          type string
                i_meta           type string optional
                i_two_pane       type abap_bool optional
                i_compact        type abap_bool optional
                "! Skip char-level inline highlighting (huge-file mode).
                i_plain          type abap_bool optional
                i_ignore_case    type abap_bool optional
                it_blame         type ty_blame_map optional
                it_blame_deleted type ty_blame_map optional
                i_code_review    type abap_bool optional
      returning value(result)    type string.

    "! Format a CDS/DDL source as HTML with syntax highlighting.
    class-methods cds_source_to_html
      importing it_source      type abaptxt255_tab
                i_title        type string
                i_meta         type string optional
      returning value(rv_html) type string.

    "! Debug rendering of diff ops and pairing decisions.
    class-methods debug_diff_html
      importing it_diff       type zif_ave_popup_types=>ty_t_diff
                i_title       type string
                i_meta        type string optional
      returning value(result) type string.

    "! Last source line number being rendered — updated during diff_to_html/debug_diff_html.
    "! Read this in a CATCH block to know which line caused a rendering error.
    class-data gv_render_line type i.

  private section.
    class-methods is_comment
      importing iv_text        type string
      returning value(rv_bool) type abap_bool.
endclass.
"! Cooperative long-running loop interrupter.
"! After `threshold_secs` of continuous work, asks the user whether to
"! continue or stop. Caller decides how to react to a Stop (e.g. break
"! out of the loop with `was_stopped( )`).
class zcl_ave_progress definition
  final
  create public.

  public section.
    methods constructor
      importing i_title          type csequence default 'Long-running operation'
                i_threshold_secs type i         default 10
                i_confirm_key    type csequence optional.

    "! Call once per iteration. Returns abap_true → caller should stop.
    "! i_remaining is used both for the SAPGUI progress bar percentage
    "! (together with i_total) and for the confirmation text.
    methods check
      importing i_remaining   type i optional
                i_total       type i optional
                i_text        type csequence optional
      returning value(result) type abap_bool.

    methods was_stopped
      returning value(result) type abap_bool.

  private section.
    data mv_title     type string.
    data mv_confirm_key type string.
    data mv_threshold type i.
    data mv_ts_start    type timestampl.
    data mv_ts_last_bar type timestampl.
    data mv_stopped     type abap_bool.
    class-data mt_confirmed_keys type hashed table of string with unique key table_line.
endclass.
"! Represents an SAP transport request — reads E070/E071 data
class zcl_ave_request definition
  final
  create public.

  public section.

    data id          type trkorr    read-only.
    data description type as4text   read-only.
    data status      type trstatus  read-only.

    methods constructor
      importing
        !id type trkorr
      raising
        zcx_ave.

    "! Returns the task (E070) most likely responsible for the given object.
    "! Prefers single-task requests; falls back to E071 lookup.
    methods get_task_for_object
      importing
                object_type   type versobjtyp
                object_name   type versobjnam
                version_date  type as4date optional
                version_time  type as4time optional
      returning value(result) type e070.

  protected section.
  private section.

    methods populate_details
      importing
        !id type trkorr
      raising
        zcx_ave.

    methods get_latest_task_for_object
      importing
                object_type   type versobjtyp
                object_name   type versobjnam
                version_date  type as4date optional
                version_time  type as4time optional
      returning value(result) type e070.

endclass.
"! Represents one version of a versionable object part.
"! Loads metadata from VRSD and source code via SVRS_GET_REPS_FROM_OBJECT.
class zcl_ave_version definition
  final
  create public.

  public section.

    constants:
      begin of c_version,
        latest_db type versno value 0,
        latest    type versno value 99998,
        active    type versno value 99998,
        modified  type versno value 99999,
      end of c_version.

    data version_number type versno      read-only.
    data request        type verskorrno  read-only.
    data task           type verskorrno  read-only.
    data author         type versuser    read-only.
    data author_name    type ad_namtext  read-only.
    data date           type versdate    read-only.
    data time           type verstime    read-only.
    data objtype        type versobjtyp  read-only.
    data objname        type versobjnam  read-only.

    methods constructor
      importing
        !vrsd type vrsd
      raising
        zcx_ave.

    "! Loads and returns the raw source code for this version
    methods get_source
      returning
        value(result) type abaptxt255_tab
      raising
        zcx_ave.

    "! Loads DDLS source via cl_svrs_tlogo_controller for any caller.
    "! i_versno is the EXTERNAL version number (e.g. 99998 for active, 00001 etc.).
    class-methods load_ddls_source
      importing i_objname     type versobjnam
                i_versno      type versno
      returning value(result) type abaptxt255_tab.

  private section.

    data vrsd type vrsd.

    methods load_attributes.

    "! Overwrite author/date/time from the task if possible
    "! (task owner better reflects who actually changed the code)
    methods load_latest_task
      raising zcx_ave.

    methods load_author_name
      raising zcx_ave.

endclass.
"! Converts between internal (DB) and external version numbers.
"! In the DB the latest version is stored as 0, but externally we use 99998
"! so that versions sort correctly (latest = highest).
class zcl_ave_versno definition
  final
  create private.

  public section.

    class-methods to_internal
      importing
                versno        type versno
      returning value(result) type versno.

    class-methods to_external
      importing
                versno        type versno
      returning value(result) type versno.

endclass.
"! Loads all VRSD records for a given object type/name.
"! Also appends artificial entries for the active (unreleased) and
"! modified (in-memory) versions, mirroring abapTimeMachine logic.
class zcl_ave_vrsd definition
  final
  create public.

  public section.

    data vrsd_list type vrsd_tab read-only.

    methods constructor
      importing
        !type             type versobjtyp
        !name             type versobjnam
        ignore_unreleased type abap_bool default abap_false
        no_toc            type abap_bool default abap_false
        date_from         type versdate  default '00000000'.

  protected section.
  private section.

    data type      type versobjtyp.
    data name      type versobjnam.
    data no_toc    type abap_bool.
    data date_from type versdate.
    data request_active_modif type trkorr.

    methods load_from_table
      importing ignore_unreleased type abap_bool.

    methods load_active_or_modified
      importing versno type versno
      raising   zcx_ave.

    methods get_request_active_modif
      returning value(result) type trkorr
      raising   zcx_ave.

    methods determine_request_active_modif
      returning value(result) type trkorr
      raising   zcx_ave.

    methods get_versionable_object
      returning value(result) type svrs2_versionable_object.

    methods get_versionable_object_mode
      importing versno        type versno
      returning value(result) type char1.

    methods read_vrsd
      importing versno        type versno
      returning value(result) type vrsd
      raising   zcx_ave.

endclass.
class zcl_ave_vrsd implementation.
  method constructor.
    me->type      = type.
    me->name      = name.
    me->no_toc    = no_toc.
    me->date_from = date_from.
    load_from_table( ignore_unreleased ).
    if ignore_unreleased = abap_false.
      try.
          load_active_or_modified( zcl_ave_version=>c_version-active ).
          " Modified (not-yet-activated workbench state) is intentionally skipped
        catch zcx_ave.
          " Object type not supported (e.g. CPUB, METH)
          " Released versions from DB are still available
      endtry.
    endif.
    sort me->vrsd_list by versno ascending.
  endmethod.
  method load_from_table.
    data versno_range type range of versno.
    if ignore_unreleased = abap_true.
      versno_range = value #( sign = 'I' option = 'NE' ( low = '00000' ) ).
    endif.

    data lt_trtype type range of char1.
    if me->no_toc = abap_true.
      append value #( sign = 'E' option = 'EQ' low = 'T' ) to lt_trtype.
    endif.

    select v~* from vrsd as v
      inner join e070 as e on e~trkorr = v~korrnum
      where v~objtype = @me->type
        and v~objname = @me->name
        and v~versno in @versno_range
        and v~datum >= @me->date_from
        and e~trfunction in @lt_trtype
      order by v~versno
      into table @me->vrsd_list.
    " Convert internal 0 → external 99998 for consistent sorting
    loop at me->vrsd_list reference into data(vrsd).
      vrsd->versno = zcl_ave_versno=>to_external( vrsd->versno ).
    endloop.

    " Supplement from SVRS_GET_VERSION_DIRECTORY_46 — accepts full OBJNAME (LIKE VRSD-OBJNAME)
    " and returns VERSION_LIST LIKE VRSD. Covers versions not yet written to VRSD
    " (e.g. activated into an unreleased task). Works for long names (METH ≤110 chars).
    data lt_dir46    type vrsd_tab.
    data lt_lversno type table of vrsn.
    call function 'SVRS_GET_VERSION_DIRECTORY_46'
      exporting
        objtype      = me->type
        objname      = me->name
      tables
        lversno_list = lt_lversno
        version_list = lt_dir46
      exceptions
        no_entry     = 1
        others       = 2.
    if sy-subrc = 0.
      loop at lt_dir46 reference into data(ls_dir46).
        " Skip active (00000) and modified (99997) — handled by load_active_or_modified
        if ls_dir46->versno = '00000' or ls_dir46->versno = '99997'.
          continue.
        endif.
        " Apply date_from filter
        if me->date_from <> '00000000' and ls_dir46->datum < me->date_from.
          continue.
        endif.
        " Apply no_toc filter (skip TOC entries)
        if me->no_toc = abap_true.
          data ls_e070_dir type e070.
          select single * from e070 where trkorr = @ls_dir46->korrnum
            into @ls_e070_dir.
          if ls_e070_dir-trfunction = 'T'.
            continue.
          endif.
        endif.
        " Skip if already loaded from VRSD
        data(lv_ext) = zcl_ave_versno=>to_external( ls_dir46->versno ).
        read table me->vrsd_list with key versno = lv_ext transporting no fields.
        if sy-subrc <> 0.
          ls_dir46->versno = lv_ext.
          insert ls_dir46->* into table me->vrsd_list.
        endif.
      endloop.
    endif.
  endmethod.
  method load_active_or_modified.
    data ls_vrsd type vrsd.

    if versno = zcl_ave_version=>c_version-active.
      " Use SVRS_GET_VERSION_DIRECTORY_46 — accepts full OBJNAME (LIKE VRSD-OBJNAME),
      " works for both short (PROG/REPS) and long (METH ≤110 chars) names.
      " versno='00000' in the result = active version with exact korrnum/datum/zeit/author.
      " Do NOT use read_vrsd/SVRS_GET_VERSION_REPOSITORY mode='A' — it may return
      " metadata of the last activated virtual version (e.g. version 19 data).
      data lt_dir_a  type vrsd_tab.
      data lt_lv_a   type table of vrsn.
      call function 'SVRS_GET_VERSION_DIRECTORY_46'
        exporting
          objtype      = me->type
          objname      = me->name
        tables
          lversno_list = lt_lv_a
          version_list = lt_dir_a
        exceptions
          no_entry     = 1
          others       = 2.
      if sy-subrc <> 0.
        return.
      endif.
      " Active version stored internally as versno='00000'
      read table lt_dir_a into data(ls_a0)
        with key versno = '00000'.
      if sy-subrc <> 0.
        return.
      endif.
      ls_vrsd-versno  = versno.   " our external key: 99998
      ls_vrsd-objtype = me->type.
      ls_vrsd-objname = me->name.
      ls_vrsd-korrnum = ls_a0-korrnum.
      ls_vrsd-datum   = ls_a0-datum.
      ls_vrsd-zeit    = ls_a0-zeit.
      ls_vrsd-author  = ls_a0-author.
    else.
      " Modified or other special version — use repository + lock detection
      ls_vrsd = read_vrsd( versno ).
      if ls_vrsd is initial or ls_vrsd-author is initial.
        return.
      endif.
      ls_vrsd-versno  = versno.
      ls_vrsd-objtype = me->type.
      ls_vrsd-objname = me->name.
      ls_vrsd-korrnum = get_request_active_modif( ).
    endif.

    read table me->vrsd_list assigning field-symbol(<existing>)
      with key versno = versno.
    if sy-subrc = 0.
      <existing>-korrnum = ls_vrsd-korrnum.
      <existing>-datum   = ls_vrsd-datum.
      <existing>-zeit    = ls_vrsd-zeit.
      <existing>-author  = ls_vrsd-author.
    else.
      insert ls_vrsd into table me->vrsd_list.
    endif.
  endmethod.
  method determine_request_active_modif.
    data s_ko100   type ko100.
    data locked    type trparflag.
    data s_tlock   type tlock.
    data s_tlock_key type tlock_int.

    call function 'TR_GET_PGMID_FOR_OBJECT'
      exporting
        iv_object      = me->type
      importing
        es_type        = s_ko100
      exceptions
        illegal_object = 1
        others         = 2.
    if sy-subrc <> 0.
      raise exception type zcx_ave.
    endif.

    data(s_e071) = value e071(
      pgmid    = s_ko100-pgmid
      object   = me->type
      obj_name = me->name ).

    call function 'TR_CHECK_TYPE'
      exporting
        wi_e071     = s_e071
      importing
        pe_result   = locked
        we_lock_key = s_tlock_key.
    if locked <> 'L'.
      return.
    endif.

    call function 'TRINT_CHECK_LOCKS'
      exporting
        wi_lock_key = s_tlock_key
      importing
        we_lockflag = locked
        we_tlock    = s_tlock
      exceptions
        empty_key   = 1
        others      = 2.
    if sy-subrc <> 0.
      zcx_ave=>raise_from_syst( ).
    endif.

    if locked is initial.
      return.
    endif.

    result = s_tlock-trkorr.
  endmethod.
  method get_request_active_modif.
    if me->request_active_modif is initial.
      me->request_active_modif = determine_request_active_modif( ).
    endif.
    result = me->request_active_modif.
  endmethod.
  method read_vrsd.
    call function 'SVRS_INITIALIZE_DATAPOINTER'
      changing
        objtype      = me->type
        data_pointer = me->type.

    data(obj) = get_versionable_object( ).
    call function 'SVRS_GET_VERSION_REPOSITORY'
      exporting
        mode      = get_versionable_object_mode( versno )
      changing
        obj       = obj
      exceptions
        not_found = 1
        others    = 2.
    if sy-subrc <> 0.
      return.
    endif.

    call function 'SVRS_EXTRACT_INFO_FROM_OBJECT'
      exporting
        object    = obj
      changing
        vrsd_info = result.
  endmethod.
  method get_versionable_object.
    result = value #(
      objtype      = me->type
      data_pointer = me->type
      objname      = me->name
      header_only  = abap_true ).
  endmethod.
  method get_versionable_object_mode.
    result = switch #(
      versno
      when zcl_ave_version=>c_version-active   then 'A'
      when zcl_ave_version=>c_version-modified then 'M' ).
  endmethod.
endclass.

class zcl_ave_versno implementation.

  method to_internal.
    " 99998 = active/latest externally → 0 in DB
    result = cond #(
      when versno = 99998 then 0
      else versno ).
  endmethod.

  method to_external.
    " 0 in DB → 99998 externally (sorts after real versions)
    result = cond #(
      when versno = 0 then 99998
      else versno ).
  endmethod.

endclass.

class zcl_ave_version implementation.

  method constructor.
    me->vrsd = vrsd.
    load_attributes( ).
    load_latest_task( ).
    load_author_name( ).
  endmethod.

  method get_source.
    if vrsd-objtype = 'DDLS'.
      result = load_ddls_source(
        i_objname = vrsd-objname
        i_versno  = me->version_number ).
      return.
    endif.

    data lt_trdir type trdir_it.

    call function 'SVRS_GET_REPS_FROM_OBJECT'
      exporting
        object_name = vrsd-objname
        object_type = vrsd-objtype
        versno      = zcl_ave_versno=>to_internal( me->version_number )
      tables
        repos_tab   = result
        trdir_tab   = lt_trdir
      exceptions
        no_version  = 1
        others      = 2.
    " subrc <> 0 → empty source, not treated as error
  endmethod.
  method load_ddls_source.
    data: lo_controller type ref to cl_svrs_tlogo_controller,
          lo_db_view    type ref to cl_svrs_tlogo_db_view,
          lo_log_view   type ref to cl_svrs_tlogo_log_view.
    field-symbols: <content> type any,
                   <ddlsrc>  type any table,
                   <row>     type any,
                   <field>   type any.
    try.
        create object lo_controller.
        lo_db_view = lo_controller->get_object(
          iv_objtype     = 'DDLS'
          iv_objname     = i_objname
          iv_versno      = i_versno
          iv_destination = '' ).
        check lo_db_view is bound.
        lo_log_view = lo_db_view->convert_to_log_view( ).
        check lo_log_view is bound and lo_log_view->ar_content is bound.
        assign lo_log_view->ar_content->* to <content>.
        check sy-subrc = 0.
        assign component 'DDLSOURCE' of structure <content> to <ddlsrc>.
        check sy-subrc = 0.
        loop at <ddlsrc> assigning <row>.
          assign component 1 of structure <row> to <field>.
          if sy-subrc = 0.
            data lv_line type string.
            lv_line = <field>.
            append conv abaptxt255( lv_line ) to result.
          endif.
        endloop.
      catch cx_root.
    endtry.
  endmethod.

  method load_attributes.
    me->version_number = vrsd-versno.
    me->author         = vrsd-author.
    me->date           = vrsd-datum.
    me->time           = vrsd-zeit.
    me->request        = vrsd-korrnum.
    me->objtype        = vrsd-objtype.
    me->objname        = vrsd-objname.
  endmethod.

  method load_latest_task.
    if me->request is initial.
      return.
    endif.

    " Active version (99998): date/time/author already set correctly from
    " SVRS_GET_VERSION_DIRECTORY in zcl_ave_vrsd — don't overwrite with task data.
    if me->version_number = c_version-active.
      return.
    endif.

    " korrnum is a request — find the responsible task within it
    data(lo_request) = new zcl_ave_request( me->request ).
    data(ls_e070) = lo_request->get_task_for_object(
      object_type  = vrsd-objtype
      object_name  = vrsd-objname
      version_date = me->date
      version_time = me->time ).
    if ls_e070-trkorr is not initial.
      me->task   = ls_e070-trkorr.
      me->author = ls_e070-as4user.
*      me->date   = ls_e070-as4date.
*      me->time   = ls_e070-as4time.
    endif.
  endmethod.

  method load_author_name.
    me->author_name = new zcl_ave_author( )->get_name( me->author ).
  endmethod.

endclass.

class zcl_ave_request implementation.
  method constructor.
    me->id = id.
    populate_details( id ).
  endmethod.
  method populate_details.
    select as4text, trstatus into (@description, @status)
      up to 1 rows
      from e070
      left join e07t on e07t~trkorr = e070~trkorr
      where e070~trkorr = @id
      order by as4text, trstatus.
      exit.
    endselect.
    " E070 may be empty in sandbox/copy systems — silently ignore.
  endmethod.
  method get_task_for_object.
    data(lv_object_type) = switch versobjtyp( object_type
      when 'REPS' or 'REPT' then 'PROG'
      when 'CINC' or 'CLSD' or
           'CPUB' or 'CPRO' or 'CPRI' then 'CLAS'
      else object_type ).
    data(lv_object_name) = object_name.
    case object_type.
      when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' or 'REPT'.
        data(lv_eq) = find( val = lv_object_name sub = '=' ).
        if lv_eq > 0.
          lv_object_name = lv_object_name(lv_eq).
        endif.
    endcase.

    result = get_latest_task_for_object(
      object_type  = lv_object_type
      object_name  = lv_object_name
      version_date = version_date
      version_time = version_time ).
  endmethod.
  method get_latest_task_for_object.
    data(lv_trf_s) = conv e070-trfunction( 'S' ).
    data lv_request_trfunction type e070-trfunction.
    data lt_tasks type standard table of e070.
    types: begin of ty_obj_key,
             object   type e071-object,
             obj_name type e071-obj_name,
           end of ty_obj_key.
    data lt_keys type sorted table of ty_obj_key with unique key object obj_name.

    insert value #( object = object_type obj_name = object_name ) into table lt_keys.
    if object_type = 'PROG'.
      insert value #( object = 'REPS' obj_name = object_name ) into table lt_keys.
    elseif object_type = 'REPS'.
      insert value #( object = 'PROG' obj_name = object_name ) into table lt_keys.
    endif.

    select single trfunction from e070
      where trkorr = @me->id
      into @lv_request_trfunction.

    select e070~trkorr, e070~strkorr, e070~as4user, e070~as4date, e070~as4time
      from e071
      inner join e070 on e070~trkorr = e071~trkorr
      for all entries in @lt_keys
      where e071~object     = @lt_keys-object
        and e071~obj_name   = @lt_keys-obj_name
        and e070~trfunction = @lv_trf_s
      into corresponding fields of table @lt_tasks.

    sort lt_tasks by as4date descending as4time descending.
    loop at lt_tasks into data(ls_task).
      check version_date is initial
         or ls_task-as4date < version_date
         or ( ls_task-as4date = version_date and ls_task-as4time <= version_time ).
      check lv_request_trfunction <> 'K' or ls_task-strkorr = me->id.
      result = ls_task.
      exit.
    endloop.
  endmethod.
endclass.

class zcl_ave_progress implementation.

  method constructor.
    mv_title     = i_title.
    mv_confirm_key = cond string(
      when i_confirm_key is not initial then i_confirm_key
      else conv string( i_title ) ).
    mv_threshold = i_threshold_secs.
    get time stamp field mv_ts_start.
    mv_ts_last_bar = mv_ts_start.
  endmethod.

  method check.
    if mv_stopped = abap_true.
      result = abap_true.
      return.
    endif.

    data lv_now  type timestampl.
    data lv_secs type tzntstmpl.
    get time stamp field lv_now.

    " SAPGUI progress bar — throttle to once per second so cheap iterations
    " don't flood the GUI with roundtrips.
    cl_abap_tstmp=>subtract(
      exporting
        tstmp1 = lv_now
        tstmp2 = mv_ts_last_bar
      receiving
        r_secs = lv_secs ).
    if lv_secs >= 1 and i_total > 0 and i_remaining >= 0.
      data(lv_done) = i_total - i_remaining.
      data(lv_pct)  = conv i( lv_done * 100 / i_total ).

      " ETA: elapsed * remaining / done
      data lv_elapsed type tzntstmpl.
      cl_abap_tstmp=>subtract(
        exporting
          tstmp1 = lv_now
          tstmp2 = mv_ts_start
        receiving
          r_secs = lv_elapsed ).
      data(lv_eta) = ``.
      if lv_done > 0 and lv_elapsed > 0.
        data(lv_eta_secs) = conv i( lv_elapsed * i_remaining / lv_done ).
        data(lv_min) = lv_eta_secs div 60.
        data(lv_sec) = lv_eta_secs mod 60.
        lv_eta = cond string(
          when lv_min > 0 then | – est. { lv_min }m { lv_sec }s left|
          else                 | – est. { lv_sec }s left| ).
      endif.

      data(lv_msg)  = cond string(
        when i_text is not initial then |{ i_text } ({ lv_done }/{ i_total }){ lv_eta }|
        else                            |{ mv_title } ({ lv_done }/{ i_total }){ lv_eta }| ).
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = lv_pct
          text       = conv char70( lv_msg ).
      mv_ts_last_bar = lv_now.
    endif.

    " Threshold check: ask the user whether to keep going
    cl_abap_tstmp=>subtract(
      exporting
        tstmp1 = lv_now
        tstmp2 = mv_ts_start
      receiving
        r_secs = lv_secs ).
    if lv_secs <= mv_threshold.
      return.
    endif.
    if line_exists( mt_confirmed_keys[ table_line = mv_confirm_key ] ).
      get time stamp field mv_ts_start.
      return.
    endif.

    data(lv_text) = cond string(
      when i_remaining > 0 then |{ i_remaining } items remaining. Continue?|
      else                      |Operation is taking a while. Continue?| ).
    data lv_answer type c length 1.
    call function 'POPUP_TO_CONFIRM'
      exporting
        titlebar       = conv char70( mv_title )
        text_question  = lv_text
        text_button_1  = 'Continue'
        text_button_2  = 'Stop'
        default_button = '2'
        start_column   = 60
        start_row      = 3
      importing
        answer         = lv_answer.
    if lv_answer <> '1'.
      mv_stopped = abap_true.
      result     = abap_true.
      return.
    endif.
    insert mv_confirm_key into table mt_confirmed_keys.
    get time stamp field mv_ts_start.
  endmethod.

  method was_stopped.
    result = mv_stopped.
  endmethod.

endclass.

class zcl_ave_popup_html implementation.

  method is_comment.
    data(lv_t) = condense( val = iv_text ).
    rv_bool = boolc( strlen( lv_t ) > 0 and ( lv_t(1) = `"` or lv_t(1) = `*` ) ).
  endmethod.

  method source_to_html.
    data lv_rows type string.
    data lv_lno  type i.

    loop at it_source into data(ls_src).
      lv_lno += 1.
      data(lv_line) = conv string( ls_src ).
      replace all occurrences of `&` in lv_line with `&amp;`.
      replace all occurrences of `<` in lv_line with `&lt;`.
      replace all occurrences of `>` in lv_line with `&gt;`.
      lv_rows = lv_rows &&
        |<tr><td class="ln">{ lv_lno }</td>| &&
        |<td class="cd">{ lv_line }</td></tr>|.
    endloop.

    rv_html =
      |<!DOCTYPE html><html><head><meta charset="utf-8"><style>| &&
      |*\{margin:0;padding:0;box-sizing:border-box\}| &&
      |body\{background:#ffffff;color:#1e1e1e;font:12px/1.5 Consolas,monospace\}| &&
      |.hdr\{background:#f3f3f3;padding:5px 12px;border-bottom:1px solid #ddd;| &&
             |color:#444;font-size:11px;display:flex;gap:16px;flex-wrap:wrap\}| &&
      |.ttl\{color:#0066aa;font-weight:bold\}| &&
      |.meta\{color:#888\}| &&
      |table\{border-collapse:collapse;width:100%\}| &&
      |tr:hover td\{background:#f0f4fa\}| &&
      |.ln\{color:#aaa;text-align:right;padding:1px 10px 1px 5px;| &&
           |user-select:none;min-width:42px;border-right:1px solid #e0e0e0;| &&
           |white-space:nowrap;background:#fafafa\}| &&
      |.cd\{padding:1px 8px;white-space:pre\}| &&
      |</style></head><body>| &&
      |<div class="hdr">| &&
      |<span class="ttl">| && i_title && |</span>| &&
      |<span class="meta">| && i_meta  && |</span>| &&
      |</div>| &&
      |<table><tbody>| && lv_rows &&
      |</tbody></table></body></html>|.
  endmethod.
  method diff_to_html.
    data lv_rows  type string.
    data lv_lno   type i.

    " Pre-compute which '=' lines to show in compact mode (within 3 of any change)
    constants lc_ctx type i value 3.
    data lt_show type table of abap_bool with default key.
    data(lv_ntot) = lines( it_diff ).
    do lv_ntot times. append abap_false to lt_show. enddo.
    if i_compact = abap_true.
      data lv_ci type i.
      lv_ci = 1.
      loop at it_diff into data(ls_cm).
        if ls_cm-op = '-' or ls_cm-op = '+'.
          data lv_from type i.
          data lv_to   type i.
          lv_from = lv_ci - lc_ctx.
          lv_to   = lv_ci + lc_ctx.
          if lv_from < 1. lv_from = 1. endif.
          if lv_to > lv_ntot. lv_to = lv_ntot. endif.
          data lv_fi type i.
          lv_fi = lv_from.
          while lv_fi <= lv_to.
            lt_show[ lv_fi ] = abap_true.
            lv_fi += 1.
          endwhile.
        endif.
        lv_ci += 1.
      endloop.
    endif.

    data(lo_progress) = new zcl_ave_progress(
      i_title          = 'Rendering diff'
      i_threshold_secs = 15 ).

    if i_two_pane = abap_true.
      " ── Two-pane rendering ──────────────────────────────────────
      data lv_lno_l type i.
      data lv_lno_r type i.
      data lv_max_w type i.
      data lv_pos2  type i value 1.
      data lv_tot2  type i.
      lv_tot2 = lines( it_diff ).

      " Calculate max line length of left (base/new) content for column width
      loop at it_diff into data(ls_w) where op = '=' or op = '+'.
        data(lv_wl) = strlen( condense( val = conv string( ls_w-text ) ) ).
        if lv_wl > lv_max_w. lv_max_w = lv_wl. endif.
      endloop.
      lv_max_w = lv_max_w + 4.   " small padding

      data lv_gap2 type abap_bool.
      while lv_pos2 <= lv_tot2.
        if lo_progress->check(
             i_remaining = lv_tot2 - lv_pos2 + 1
             i_total     = lv_tot2 ) = abap_true.
          exit.
        endif.
        read table it_diff into data(ls_c2) index lv_pos2.

        if ls_c2-op = '='.
          lv_lno_l += 1. lv_lno_r += 1.
          if i_compact = abap_true and lt_show[ lv_pos2 ] = abap_false.
            if lv_gap2 = abap_false.
              lv_rows = lv_rows &&
                |<tr style="background:#f0f0f0;color:#888">| &&
                |<td class="ln">...</td><td class="cd">...</td>| &&
                |<td class="sep"></td>| &&
                |<td class="ln">...</td><td class="cd">...</td></tr>|.
              lv_gap2 = abap_true.
            endif.
            lv_pos2 += 1.
            continue.
          endif.
          clear lv_gap2.
          data(lv_eq2) = ls_c2-text.
          replace all occurrences of `&` in lv_eq2 with `&amp;`.
          replace all occurrences of `<` in lv_eq2 with `&lt;`.
          replace all occurrences of `>` in lv_eq2 with `&gt;`.
          data(lv_cmt_eq2) = cond string( when is_comment( ls_c2-text ) = abap_true
            then ` style="background:#fafae8"` else `` ).
          lv_rows = lv_rows &&
            |<tr><td class="ln">{ lv_lno_l }</td>| &&
            |<td class="cd"{ lv_cmt_eq2 }>{ lv_eq2 }</td>| &&
            |<td class="sep"></td>| &&
            |<td class="ln">{ lv_lno_r }</td>| &&
            |<td class="cd"{ lv_cmt_eq2 }>{ lv_eq2 }</td></tr>|.
          lv_pos2 += 1.

        elseif ls_c2-op = '-' or ls_c2-op = '+'.
          data lt_d2 type string_table.
          data lt_i2 type string_table.
          data lv_sc type i.
          lv_sc = lv_pos2.
          " Extended block: collect '-'/'+' AND short bridging empty '=' lines
          " (max 1 in a row) when more changes follow. Bridged '=' lines are
          " not added to lt_d2/lt_i2 (they're equal on both sides) but still
          " advance lv_sc so pairing across the gap works.
          while lv_sc <= lv_tot2.
            read table it_diff into data(ls_s2) index lv_sc.
            if ls_s2-op = '-'.
              append ls_s2-text to lt_d2. lv_sc += 1.
            elseif ls_s2-op = '+'.
              append ls_s2-text to lt_i2. lv_sc += 1.
            elseif ls_s2-op = '=' and condense( val = ls_s2-text ) = ``.
              data lv_peek2  type i.
              data lv_extra2 type i.
              data lv_more2  type abap_bool.
              lv_peek2 = lv_sc + 1.
              lv_extra2 = 0.
              lv_more2 = abap_false.
              while lv_peek2 <= lv_tot2.
                read table it_diff into data(ls_p2) index lv_peek2.
                if ls_p2-op = '-' or ls_p2-op = '+'.
                  lv_more2 = abap_true.
                  exit.
                elseif ls_p2-op = '=' and condense( val = ls_p2-text ) = `` and lv_extra2 < 1.
                  lv_extra2 += 1.
                  lv_peek2 += 1.
                  continue.
                else.
                  exit.
                endif.
              endwhile.
              if lv_more2 = abap_true.
                lv_sc += 1.
              else.
                exit.
              endif.
            else.
              exit.
            endif.
          endwhile.
          data(lv_nd) = lines( lt_d2 ).
          data(lv_ni) = lines( lt_i2 ).

          " Blame separator for two-pane (added lines)
          if it_blame is not initial and lt_i2 is not initial.
            read table it_blame into data(ls_bl2) with key text = lt_i2[ 1 ].
            if sy-subrc = 0.
              data(lv_bdate2) = |{ ls_bl2-datum+6(2) }.{ ls_bl2-datum+4(2) }.{ ls_bl2-datum(4) }|.
              data(lv_btime2) = |{ ls_bl2-zeit(2) }:{ ls_bl2-zeit+2(2) }|.
              data(lv_btask2) = cond string(
                when ls_bl2-korrnum is not initial and ls_bl2-task is not initial then | { ls_bl2-korrnum }/{ ls_bl2-task }|
                when ls_bl2-korrnum is not initial then | { ls_bl2-korrnum }|
                when ls_bl2-task is not initial then | { ls_bl2-task }|
                else `` ).
              data(lv_btasktxt2) = cond string( when ls_bl2-task_text is not initial then | { ls_bl2-task_text }| else `` ).
              data(lv_bauth2) = ls_bl2-author &&
                cond string( when ls_bl2-author_name is not initial then | ({ ls_bl2-author_name })| else `` ).
              data(lv_bverb2) = cond string( when lv_nd = 0 then 'inserted' else 'changed' ).
              data(lv_bline2s) = |── { lv_bauth2 } { lv_bverb2 }  { lv_bdate2 }| &&
                | { lv_btime2 }  v.{ ls_bl2-versno_text } ──|.
              data(lv_bline2) = |── { lv_bauth2 } { lv_bverb2 }  { lv_bdate2 }| &&
                | { lv_btime2 }  v.{ ls_bl2-versno_text }{ lv_btask2 }{ lv_btasktxt2 } ──|.
              if strlen( ls_bl2-task_text ) > 10.
                " Split: first row without TR info, second row with TR info only
                lv_rows = lv_rows &&
                  |<tr style="background:#e8f4e8;color:#555;font-size:10px;font-style:italic">| &&
                  |<td class="ln">▶</td><td class="cd" colspan="3">{ lv_bline2s }</td>| &&
                  |<td class="ln"></td><td class="cd"></td></tr>| &&
                  |<tr style="background:#e8f4e8;color:#555;font-size:10px;font-style:italic">| &&
                  |<td class="ln"></td><td class="cd" colspan="3">──{ lv_btask2 }{ lv_btasktxt2 } ──</td>| &&
                  |<td class="ln"></td><td class="cd"></td></tr>|.
              else.
                lv_rows = lv_rows &&
                  |<tr style="background:#e8f4e8;color:#555;font-size:10px;font-style:italic">| &&
                  |<td class="ln">▶</td><td class="cd" colspan="3">{ lv_bline2 }</td>| &&
                  |<td class="ln"></td><td class="cd"></td></tr>|.
              endif.
            endif.
          endif.
          " Blame separator for two-pane (deleted lines)
          if it_blame_deleted is not initial and lt_d2 is not initial and lt_i2 is initial.
            read table it_blame_deleted into data(ls_bld2) with key text = lt_d2[ 1 ].
            if sy-subrc = 0.
              data(lv_bddate2) = |{ ls_bld2-datum+6(2) }.{ ls_bld2-datum+4(2) }.{ ls_bld2-datum(4) }|.
              data(lv_bdtime2) = |{ ls_bld2-zeit(2) }:{ ls_bld2-zeit+2(2) }|.
              data(lv_bdtask2) = cond string(
                when ls_bld2-korrnum is not initial and ls_bld2-task is not initial then | { ls_bld2-korrnum }/{ ls_bld2-task }|
                when ls_bld2-korrnum is not initial then | { ls_bld2-korrnum }|
                when ls_bld2-task is not initial then | { ls_bld2-task }|
                else `` ).
              data(lv_bdtasktxt2) = cond string( when ls_bld2-task_text is not initial then | { ls_bld2-task_text }| else `` ).
              data(lv_bdauth2) = ls_bld2-author &&
                cond string( when ls_bld2-author_name is not initial then | ({ ls_bld2-author_name })| else `` ).
              data(lv_bdline2) = |── { lv_bdauth2 } deleted  { lv_bddate2 } { lv_bdtime2 }  v.{ ls_bld2-versno_text }{ lv_bdtask2 }{ lv_bdtasktxt2 } ──|.
              if strlen( lv_bdline2 ) > lv_max_w and ( lv_bdtask2 is not initial or lv_bdtasktxt2 is not initial ).
                lv_rows = lv_rows &&
                  |<tr style="background:#fdf0f0;color:#555;font-size:10px;font-style:italic;font-weight:bold">| &&
                  |<td class="ln">◀</td><td class="cd" colspan="3">── { lv_bdauth2 } deleted  { lv_bddate2 } { lv_bdtime2 }  v.{ ls_bld2-versno_text } ──</td>| &&
                  |<td class="ln"></td><td class="cd"></td></tr>| &&
                  |<tr style="background:#fdf0f0;color:#555;font-size:10px;font-style:italic;font-weight:bold">| &&
                  |<td class="ln"></td><td class="cd" colspan="3">──{ lv_bdtask2 }{ lv_bdtasktxt2 } ──</td>| &&
                  |<td class="ln"></td><td class="cd"></td></tr>|.
              else.
                lv_rows = lv_rows &&
                  |<tr style="background:#fdf0f0;color:#555;font-size:10px;font-style:italic;font-weight:bold">| &&
                  |<td class="ln">◀</td><td class="cd" colspan="3">{ lv_bdline2 }</td>| &&
                  |<td class="ln"></td><td class="cd"></td></tr>|.
              endif.
            endif.
          endif.

          data(lv_nd2) = lines( lt_d2 ).
          data(lv_ni2) = lines( lt_i2 ).

          data lt_d2_pair_idx type standard table of i with default key.
          data lt_i2_pair_idx type standard table of i with default key.
          data lt_d2_paired   type table of abap_bool with default key.
          data lt_i2_paired   type table of abap_bool with default key.
          do lv_nd2 times. append abap_false to lt_d2_paired. enddo.
          do lv_ni2 times. append abap_false to lt_i2_paired. enddo.

          if lv_nd2 > 0 and lv_ni2 > 0.
            data(lv_cols_2p) = lv_ni2 + 1.
            data(lv_rows_2p) = lv_nd2 + 1.
            data lt_dp_2p type table of i.
            data(lv_size_2p) = lv_rows_2p * lv_cols_2p.
            do lv_size_2p times.
              append 0 to lt_dp_2p.
            enddo.

            data lv_di2 type i.
            data lv_ii2 type i.
            lv_di2 = 1.
            while lv_di2 <= lv_nd2.
              lv_ii2 = 1.
              while lv_ii2 <= lv_ni2.
                data(lv_cell_2p) = lv_di2 * lv_cols_2p + lv_ii2 + 1.
                if zcl_ave_popup_diff=>has_common_chars( iv_a = lt_d2[ lv_di2 ] iv_b = lt_i2[ lv_ii2 ] ) = abap_true.
                  data(lv_prev_2p) = ( lv_di2 - 1 ) * lv_cols_2p + ( lv_ii2 - 1 ) + 1.
                  lt_dp_2p[ lv_cell_2p ] = lt_dp_2p[ lv_prev_2p ] + 1.
                else.
                  data(lv_up_2p)   = ( lv_di2 - 1 ) * lv_cols_2p + lv_ii2 + 1.
                  data(lv_left_2p) = lv_di2 * lv_cols_2p + ( lv_ii2 - 1 ) + 1.
                  lt_dp_2p[ lv_cell_2p ] = cond i(
                    when lt_dp_2p[ lv_up_2p ] >= lt_dp_2p[ lv_left_2p ] then lt_dp_2p[ lv_up_2p ]
                    else lt_dp_2p[ lv_left_2p ] ).
                endif.
                lv_ii2 += 1.
              endwhile.
              lv_di2 += 1.
            endwhile.

            lv_di2 = lv_nd2.
            lv_ii2 = lv_ni2.
            while lv_di2 > 0 and lv_ii2 > 0.
              if zcl_ave_popup_diff=>has_common_chars( iv_a = lt_d2[ lv_di2 ] iv_b = lt_i2[ lv_ii2 ] ) = abap_true.
                insert lv_di2 into lt_d2_pair_idx index 1.
                insert lv_ii2 into lt_i2_pair_idx index 1.
                lv_di2 -= 1.
                lv_ii2 -= 1.
              else.
                data(lv_up_bt2)   = ( lv_di2 - 1 ) * lv_cols_2p + lv_ii2 + 1.
                data(lv_left_bt2) = lv_di2 * lv_cols_2p + ( lv_ii2 - 1 ) + 1.
                if lt_dp_2p[ lv_up_bt2 ] >= lt_dp_2p[ lv_left_bt2 ].
                  lv_di2 -= 1.
                else.
                  lv_ii2 -= 1.
                endif.
              endif.
            endwhile.
          endif.

          data lv_dl2 type string.
          data lv_il2 type string.

          " Walk lt_i2 (new/left) and lt_d2 (old/right) in document order.
          " Rendering paired first then solos breaks line-number ordering when a
          " solo insert precedes a paired row in the new file. Instead, advance
          " both pointers together, following pair anchors, and render solos as
          " they appear in each file's natural sequence.
          data lv_di type i.
          data lv_ii type i.
          data lv_pk type i.
          lv_di = 1. lv_ii = 1. lv_pk = 1.
          data(lv_np) = lines( lt_d2_pair_idx ).
          while lv_di <= lv_nd2 or lv_ii <= lv_ni2.
            " Sentinel pair indices (beyond end when no more pairs)
            data(lv_npd) = cond i( when lv_pk <= lv_np then lt_d2_pair_idx[ lv_pk ] else lv_nd2 + 1 ).
            data(lv_npi) = cond i( when lv_pk <= lv_np then lt_i2_pair_idx[ lv_pk ] else lv_ni2 + 1 ).
            if lv_di = lv_npd and lv_ii = lv_npi.
              " Paired row: advance both counters
              lv_lno_l += 1. lv_lno_r += 1.
              if i_plain = abap_true.
                lv_dl2 = escape( val = lt_i2[ lv_ii ] format = cl_abap_format=>e_html_text ).
                lv_il2 = escape( val = lt_d2[ lv_di ] format = cl_abap_format=>e_html_text ).
              else.
                lv_dl2 = zcl_ave_popup_diff=>char_diff_html( iv_old = lt_d2[ lv_di ] iv_new = lt_i2[ lv_ii ] iv_side = 'N' iv_ignore_case = i_ignore_case ).
                lv_il2 = zcl_ave_popup_diff=>char_diff_html( iv_old = lt_d2[ lv_di ] iv_new = lt_i2[ lv_ii ] iv_side = 'O' iv_ignore_case = i_ignore_case ).
              endif.
              data(lv_cmt_l2) = cond string( when is_comment( lt_i2[ lv_ii ] ) = abap_true
                then `;background:#fafae8` else `` ).
              data(lv_cmt_r2) = cond string( when is_comment( lt_d2[ lv_di ] ) = abap_true
                then `;color:#cc0000` else `` ).
              lv_rows = lv_rows &&
                |<tr>| &&
                |<td class="ln" style="background:#eaffea">{ lv_lno_l }</td>| &&
                |<td class="cd" style="background:#eaffea{ lv_cmt_l2 }">{ lv_dl2 }</td>| &&
                |<td class="sep"></td>| &&
                |<td class="ln" style="background:#ffecec">{ lv_lno_r }</td>| &&
                |<td class="cd" style="background:#ffecec{ lv_cmt_r2 }">{ lv_il2 }</td></tr>|.
              clear: lv_dl2, lv_il2.
              lv_di += 1. lv_ii += 1. lv_pk += 1.
            elseif lv_ii < lv_npi and lv_di < lv_npd.
              " Positional pair: both sides available before next LCS anchor —
              " show side-by-side without char diff to keep document flow readable.
              lv_lno_l += 1. lv_lno_r += 1.
              lv_dl2 = lt_i2[ lv_ii ].
              lv_il2 = lt_d2[ lv_di ].
              replace all occurrences of `&` in lv_dl2 with `&amp;`.
              replace all occurrences of `<` in lv_dl2 with `&lt;`.
              replace all occurrences of `>` in lv_dl2 with `&gt;`.
              replace all occurrences of `&` in lv_il2 with `&amp;`.
              replace all occurrences of `<` in lv_il2 with `&lt;`.
              replace all occurrences of `>` in lv_il2 with `&gt;`.
              data(lv_cmt_ppl) = cond string( when is_comment( lt_i2[ lv_ii ] ) = abap_true
                then `;background:#fafae8` else `` ).
              data(lv_cmt_ppr) = cond string( when is_comment( lt_d2[ lv_di ] ) = abap_true
                then `;color:#cc0000` else `` ).
              lv_rows = lv_rows &&
                |<tr>| &&
                |<td class="ln" style="background:#eaffea">{ lv_lno_l }</td>| &&
                |<td class="cd" style="background:#eaffea{ lv_cmt_ppl }">{ lv_dl2 }</td>| &&
                |<td class="sep"></td>| &&
                |<td class="ln" style="background:#ffecec">{ lv_lno_r }</td>| &&
                |<td class="cd" style="background:#ffecec{ lv_cmt_ppr }">{ lv_il2 }</td></tr>|.
              clear: lv_dl2, lv_il2.
              lv_ii += 1. lv_di += 1.
            elseif lv_ii <= lv_ni2 and lv_ii < lv_npi.
              " Solo insert (new line, left side only)
              lv_lno_l += 1.
              lv_dl2 = lt_i2[ lv_ii ].
              replace all occurrences of `&` in lv_dl2 with `&amp;`.
              replace all occurrences of `<` in lv_dl2 with `&lt;`.
              replace all occurrences of `>` in lv_dl2 with `&gt;`.
              data(lv_cmt_si2) = cond string( when is_comment( lt_i2[ lv_ii ] ) = abap_true
                then `;background:#fafae8` else `` ).
              lv_rows = lv_rows &&
                |<tr>| &&
                |<td class="ln" style="background:#eaffea">{ lv_lno_l }</td>| &&
                |<td class="cd" style="background:#eaffea{ lv_cmt_si2 }">{ lv_dl2 }</td>| &&
                |<td class="sep"></td>| &&
                |<td class="ln"></td><td class="cd"></td></tr>|.
              clear lv_dl2.
              lv_ii += 1.
            elseif lv_di <= lv_nd2.
              " Solo delete (old line, right side only)
              lv_lno_r += 1.
              lv_il2 = lt_d2[ lv_di ].
              replace all occurrences of `&` in lv_il2 with `&amp;`.
              replace all occurrences of `<` in lv_il2 with `&lt;`.
              replace all occurrences of `>` in lv_il2 with `&gt;`.
              data(lv_cmt_sd2) = cond string( when is_comment( lt_d2[ lv_di ] ) = abap_true
                then `;color:#cc0000` else `` ).
              lv_rows = lv_rows &&
                |<tr>| &&
                |<td class="ln"></td><td class="cd"></td>| &&
                |<td class="sep"></td>| &&
                |<td class="ln" style="background:#ffecec">{ lv_lno_r }</td>| &&
                |<td class="cd" style="background:#ffecec{ lv_cmt_sd2 }">{ lv_il2 }</td></tr>|.
              clear lv_il2.
              lv_di += 1.
            else.
              " Remaining solo inserts (all dels exhausted)
              lv_lno_l += 1.
              lv_dl2 = lt_i2[ lv_ii ].
              replace all occurrences of `&` in lv_dl2 with `&amp;`.
              replace all occurrences of `<` in lv_dl2 with `&lt;`.
              replace all occurrences of `>` in lv_dl2 with `&gt;`.
              data(lv_cmt_rs2) = cond string( when is_comment( lt_i2[ lv_ii ] ) = abap_true
                then `;background:#fafae8` else `` ).
              lv_rows = lv_rows &&
                |<tr>| &&
                |<td class="ln" style="background:#eaffea">{ lv_lno_l }</td>| &&
                |<td class="cd" style="background:#eaffea{ lv_cmt_rs2 }">{ lv_dl2 }</td>| &&
                |<td class="sep"></td>| &&
                |<td class="ln"></td><td class="cd"></td></tr>|.
              clear lv_dl2.
              lv_ii += 1.
            endif.
          endwhile.

          clear: lt_d2, lt_i2, lv_gap2, lt_d2_pair_idx, lt_i2_pair_idx, lt_d2_paired, lt_i2_paired.
          lv_pos2 = lv_sc.
        else.
          lv_pos2 += 1.
        endif.
      endwhile.

      result =
        |<!DOCTYPE html><html><head><meta charset="utf-8"><style>| &&
        |*\{margin:0;padding:0;box-sizing:border-box\}| &&
        |body\{background:#fff;color:#1e1e1e;font:12px/1.5 Consolas,monospace\}| &&
        |.hdr\{background:#f3f3f3;padding:5px 56px;border-bottom:1px solid #ddd;| &&
               |color:#444;font-size:11px;display:flex;gap:8px;| &&
               |justify-content:center;align-items:center;flex-wrap:wrap\}| &&
        |.ttl\{color:#0066aa;font-weight:bold\}.meta\{color:#888\}| &&
        |table\{border-collapse:collapse;width:100%\}| &&
        |.ln\{color:#aaa;text-align:right;padding:1px 8px 1px 4px;| &&
             |user-select:none;min-width:36px;border-right:1px solid #e0e0e0;| &&
             |white-space:nowrap;background:#fafafa\}| &&
        |.cd\{padding:1px 8px;white-space:pre;width:{ lv_max_w }ch\}| &&
        |.sep\{border-left:2px solid #ccc;padding:0\}| &&
        |</style></head><body>| &&
        |<div class="hdr">| &&
        |<span class="ttl">| && i_title && |</span>| &&
        |<span class="meta">| && i_meta  && |</span>| &&
        |</div>| &&
        |<table><tbody>| && lv_rows &&
        |</tbody></table></body></html>|.
      return.
    endif.

    " ── Inline rendering (default) ───────────────────────────────

    " Scan diff ops, grouping consecutive '-' and '+' blocks
    data lv_pos   type i value 1.
    data lv_total type i.
    lv_total = lines( it_diff ).

    data lv_gap_shown type abap_bool.   " tracks if '...' separator was already output
    while lv_pos <= lv_total.
      if lo_progress->check(
           i_remaining = lv_total - lv_pos + 1
           i_total     = lv_total ) = abap_true.
        exit.
      endif.
      read table it_diff into data(ls_cur) index lv_pos.

      if ls_cur-op = '='.
        lv_lno += 1.
        gv_render_line = lv_lno.
        if i_compact = abap_true and lt_show[ lv_pos ] = abap_false.
          " Skip this line — show separator if not shown yet for this gap
          if lv_gap_shown = abap_false.
            lv_rows = lv_rows &&
              |<tr style="background:#f0f0f0;color:#888">| &&
              |<td class="ln">...</td><td class="cd">...</td></tr>|.
            lv_gap_shown = abap_true.
          endif.
          lv_pos += 1.
          continue.
        endif.
        clear lv_gap_shown.
        data(lv_line_eq) = ls_cur-text.
        replace all occurrences of `&` in lv_line_eq with `&amp;`.
        replace all occurrences of `<` in lv_line_eq with `&lt;`.
        replace all occurrences of `>` in lv_line_eq with `&gt;`.
        data(lv_cmt_eq) = cond string( when is_comment( ls_cur-text ) = abap_true
          then ` style="background:#fafae8"` else `` ).
        lv_rows = lv_rows &&
          |<tr style="background:#ffffff">| &&
          |<td class="ln">{ lv_lno }</td>| &&
          |<td class="cd"{ lv_cmt_eq }>{ lv_line_eq }</td></tr>|.
        lv_pos += 1.

      elseif ls_cur-op = '-' or ls_cur-op = '+'.
        " Collect EXTENDED block: consecutive '-'/'+' AND short bridging
        " empty '=' lines (max 1 in a row) when more changes follow.
        " This lets us pair changes across blank-line gaps that LCS inserted.
        data lt_block   type zif_ave_popup_types=>ty_t_diff.
        data lt_dels    type string_table.
        data lt_ins     type string_table.
        data lt_del_idx type standard table of i with default key.
        data lt_ins_idx type standard table of i with default key.
        data lv_scan    type i.
        clear: lt_block, lt_dels, lt_ins, lt_del_idx, lt_ins_idx.
        lv_scan = lv_pos.

        while lv_scan <= lv_total.
          read table it_diff into data(ls_s) index lv_scan.
          if ls_s-op = '-' or ls_s-op = '+'.
            append ls_s to lt_block.
            lv_scan += 1.
          elseif ls_s-op = '=' and condense( val = ls_s-text ) = ``.
            " tentative bridge — peek ahead through up to 1 more empty '='
            data lv_peek         type i.
            data lv_extra        type i.
            data lv_more_changes type abap_bool.
            lv_peek = lv_scan + 1.
            lv_extra = 0.
            lv_more_changes = abap_false.
            while lv_peek <= lv_total.
              read table it_diff into data(ls_p) index lv_peek.
              if ls_p-op = '-' or ls_p-op = '+'.
                lv_more_changes = abap_true.
                exit.
              elseif ls_p-op = '=' and condense( val = ls_p-text ) = `` and lv_extra < 1.
                lv_extra += 1.
                lv_peek += 1.
                continue.
              else.
                exit.
              endif.
            endwhile.
            if lv_more_changes = abap_true.
              append ls_s to lt_block.
              lv_scan += 1.
            else.
              exit.
            endif.
          else.
            exit.
          endif.
        endwhile.

        " Build dels/ins texts plus their positions inside lt_block.
        " Skip whitespace-only lines from pairing — they have no chars to
        " match and would otherwise eat an index slot, breaking alignment
        " between real changes. They still render as solo via the block walk.
        data lv_bi type i.
        lv_bi = 1.
        while lv_bi <= lines( lt_block ).
          data(ls_b) = lt_block[ lv_bi ].
          if ls_b-op = '-' and condense( val = ls_b-text ) <> ``.
            append ls_b-text to lt_dels.
            append lv_bi     to lt_del_idx.
          elseif ls_b-op = '+' and condense( val = ls_b-text ) <> ``.
            append ls_b-text to lt_ins.
            append lv_bi     to lt_ins_idx.
          endif.
          lv_bi += 1.
        endwhile.

        " Blame separator for added lines
        if it_blame is not initial and lt_ins is not initial.
          read table it_blame into data(ls_bl) with key text = lt_ins[ 1 ].
          if sy-subrc = 0.
            data(lv_bdate) = |{ ls_bl-datum+6(2) }.{ ls_bl-datum+4(2) }.{ ls_bl-datum(4) }|.
            data(lv_btime) = |{ ls_bl-zeit(2) }:{ ls_bl-zeit+2(2) }|.
            data(lv_btask) = cond string(
              when ls_bl-korrnum is not initial and ls_bl-task is not initial then | { ls_bl-korrnum }/{ ls_bl-task }|
              when ls_bl-korrnum is not initial then | { ls_bl-korrnum }|
              when ls_bl-task is not initial then | { ls_bl-task }|
              else `` ).
            data(lv_btasktxt) = cond string( when ls_bl-task_text is not initial then | { ls_bl-task_text }| else `` ).
            lv_rows = lv_rows &&
              |<tr style="background:#e8f4e8;color:#555;font-size:10px;font-style:italic">| &&
              |<td class="ln">▶</td>| &&
              |<td class="cd">── { ls_bl-author }| &&
              cond string( when ls_bl-author_name is not initial then | ({ ls_bl-author_name })| else `` ) &&
              | changed  { lv_bdate } { lv_btime }  v.{ ls_bl-versno_text }{ lv_btask }{ lv_btasktxt } ──</td></tr>|.
          endif.
        elseif i_code_review = abap_true and lt_ins is not initial.
          lv_rows = lv_rows &&
            `<tr style="background:#e8f4e8;color:#555;font-size:10px;font-style:italic">` &&
            `<td class="ln">▶</td>` &&
            `<td class="cd">── changed ──</td></tr>`.
        endif.
        " Blame separator for deleted lines
        if it_blame_deleted is not initial and lt_dels is not initial and lt_ins is initial.
          read table it_blame_deleted into data(ls_bld) with key text = lt_dels[ 1 ].
          if sy-subrc = 0.
            data(lv_bddate) = |{ ls_bld-datum+6(2) }.{ ls_bld-datum+4(2) }.{ ls_bld-datum(4) }|.
            data(lv_bdtime) = |{ ls_bld-zeit(2) }:{ ls_bld-zeit+2(2) }|.
            data(lv_bdtask) = cond string(
              when ls_bld-korrnum is not initial and ls_bld-task is not initial then | { ls_bld-korrnum }/{ ls_bld-task }|
              when ls_bld-korrnum is not initial then | { ls_bld-korrnum }|
              when ls_bld-task is not initial then | { ls_bld-task }|
              else `` ).
            data(lv_bdtasktxt) = cond string(
              when ls_bld-task_text is not initial then | { ls_bld-task_text }|
              else `` ).
            lv_rows = lv_rows &&
              |<tr style="background:#fdf0f0;color:#555;font-size:10px;font-style:italic;font-weight:bold">| &&
              |<td class="ln">◀</td>| &&
              |<td class="cd">── { ls_bld-author }| &&
              cond string( when ls_bld-author_name is not initial then | ({ ls_bld-author_name })| else `` ) &&
              | deleted  { lv_bddate } { lv_bdtime }  v.{ ls_bld-versno_text }| &&
              |{ lv_bdtask }{ lv_bdtasktxt } ──</td></tr>|.
          endif.
        elseif i_code_review = abap_true and lt_dels is not initial and lt_ins is initial.
          lv_rows = lv_rows &&
            `<tr style="background:#fdf0f0;color:#555;font-size:10px;font-style:italic;font-weight:bold">` &&
            `<td class="ln">◀</td>` &&
            `<td class="cd">── changed ──</td></tr>`.
        endif.

        data(lv_ndels) = lines( lt_dels ).
        data(lv_nins)  = lines( lt_ins ).

        " status[i] for each block position: 'P' = render paired here,
        "                                    'C' = consumed (skip), ' ' = solo/equal
        data lt_status      type standard table of c with default key.
        data lt_inline_html type string_table.
        clear: lt_status, lt_inline_html.
        data lv_init type i.
        lv_init = 1.
        while lv_init <= lines( lt_block ).
          append ` ` to lt_status.
          append `` to lt_inline_html.
          lv_init += 1.
        endwhile.

        if i_plain = abap_false and lv_ndels > 0 and lv_nins > 0.
          data(lv_cols_p) = lv_nins + 1.
          data(lv_rows_p) = lv_ndels + 1.
          data lt_dp_pair type table of i.
          clear lt_dp_pair.
          data(lv_size_p) = lv_rows_p * lv_cols_p.
          do lv_size_p times.
            append 0 to lt_dp_pair.
          enddo.

          data lv_di1 type i.
          data lv_ii1 type i.
          lv_di1 = 1.
          while lv_di1 <= lv_ndels.
            lv_ii1 = 1.
            while lv_ii1 <= lv_nins.
              data(lv_cell_p) = lv_di1 * lv_cols_p + lv_ii1 + 1.
              if zcl_ave_popup_diff=>has_common_chars( iv_a = lt_dels[ lv_di1 ] iv_b = lt_ins[ lv_ii1 ] ) = abap_true.
                data(lv_prev_p) = ( lv_di1 - 1 ) * lv_cols_p + ( lv_ii1 - 1 ) + 1.
                lt_dp_pair[ lv_cell_p ] = lt_dp_pair[ lv_prev_p ] + 1.
              else.
                data(lv_up_p)   = ( lv_di1 - 1 ) * lv_cols_p + lv_ii1 + 1.
                data(lv_left_p) = lv_di1 * lv_cols_p + ( lv_ii1 - 1 ) + 1.
                lt_dp_pair[ lv_cell_p ] = cond i(
                  when lt_dp_pair[ lv_up_p ] >= lt_dp_pair[ lv_left_p ] then lt_dp_pair[ lv_up_p ]
                  else lt_dp_pair[ lv_left_p ] ).
              endif.
              lv_ii1 += 1.
            endwhile.
            lv_di1 += 1.
          endwhile.

          data lt_pair_dk type standard table of i with default key.
          data lt_pair_ik type standard table of i with default key.
          clear: lt_pair_dk, lt_pair_ik.
          lv_di1 = lv_ndels.
          lv_ii1 = lv_nins.
          while lv_di1 > 0 and lv_ii1 > 0.
            if zcl_ave_popup_diff=>has_common_chars( iv_a = lt_dels[ lv_di1 ] iv_b = lt_ins[ lv_ii1 ] ) = abap_true.
              " Before taking this pair, check if skipping this ins (going left)
              " gives the same DP score — if so, prefer the earlier insertion.
              " This prevents pairing del[i] with ins[j] when ins[j-1] matches
              " equally well (e.g. 1 del + 2 ins where both have common chars).
              if lv_ii1 > 1 and
                 lt_dp_pair[ lv_di1 * lv_cols_p + ( lv_ii1 - 1 ) + 1 ] =
                 lt_dp_pair[ lv_di1 * lv_cols_p + lv_ii1 + 1 ].
                lv_ii1 -= 1.  " skip to earlier ins — same score reachable without this ins
              else.
                insert lv_di1 into lt_pair_dk index 1.
                insert lv_ii1 into lt_pair_ik index 1.
                lv_di1 -= 1.
                lv_ii1 -= 1.
              endif.
            else.
              data(lv_up_bt)   = ( lv_di1 - 1 ) * lv_cols_p + lv_ii1 + 1.
              data(lv_left_bt) = lv_di1 * lv_cols_p + ( lv_ii1 - 1 ) + 1.
              if lt_dp_pair[ lv_up_bt ] >= lt_dp_pair[ lv_left_bt ].
                lv_di1 -= 1.
              else.
                lv_ii1 -= 1.
              endif.
            endif.
          endwhile.

          lv_pk = 1.
          while lv_pk <= lines( lt_pair_dk ).
            data(lv_dk) = lt_pair_dk[ lv_pk ].
            data(lv_ik) = lt_pair_ik[ lv_pk ].
            lv_di    = lt_del_idx[ lv_dk ].
            lv_ii    = lt_ins_idx[ lv_ik ].
            data(lv_first) = cond i( when lv_di < lv_ii then lv_di else lv_ii ).
            data(lv_other) = cond i( when lv_di > lv_ii then lv_di else lv_ii ).
            lt_status[ lv_first ] = 'P'.
            lt_status[ lv_other ] = 'C'.
            lt_inline_html[ lv_first ] = zcl_ave_popup_diff=>char_diff_html(
              iv_old         = lt_dels[ lv_dk ]
              iv_new         = lt_ins[ lv_ik ]
              iv_side        = 'B'
              iv_ignore_case = i_ignore_case ).
            lv_pk += 1.
          endwhile.
        endif.
        " Render block ops in original order
        data lv_rb type i.
        lv_rb = 1.
        while lv_rb <= lines( lt_block ).
          data(ls_bo) = lt_block[ lv_rb ].
          data(lv_st) = lt_status[ lv_rb ].
          data(lv_cmt_b) = cond string( when is_comment( ls_bo-text ) = abap_true
            then `;background:#fafae8` else `` ).
          if ls_bo-op = '='.
            lv_lno += 1.
            data(lv_eq) = ls_bo-text.
            replace all occurrences of `&` in lv_eq with `&amp;`.
            replace all occurrences of `<` in lv_eq with `&lt;`.
            replace all occurrences of `>` in lv_eq with `&gt;`.
            lv_rows = lv_rows &&
              |<tr style="background:#ffffff">| &&
              |<td class="ln">{ lv_lno }</td>| &&
              |<td class="cd" style="background:#ffffff{ lv_cmt_b }">{ lv_eq }</td></tr>|.
          elseif ls_bo-op = '-'.
            if lv_st = 'P'.
              lv_lno += 1.
              lv_rows = lv_rows &&
                |<tr style="background:#ffffff">| &&
                |<td class="ln">{ lv_lno }</td>| &&
                |<td class="cd" style="background:#ffffff{ lv_cmt_b }">{ lt_inline_html[ lv_rb ] }</td></tr>|.
            elseif lv_st = 'C'.
              " skip — already rendered as part of paired row
            else.
              data(lv_dl) = ls_bo-text.
              replace all occurrences of `&` in lv_dl with `&amp;`.
              replace all occurrences of `<` in lv_dl with `&lt;`.
              replace all occurrences of `>` in lv_dl with `&gt;`.
              lv_rows = lv_rows &&
                |<tr style="background:#ffecec">| &&
                |<td class="ln" style="color:#cc0000">-</td>| &&
                |<td class="cd" style="color:#cc0000{ lv_cmt_b }">{ lv_dl }</td></tr>|.
            endif.
          else.  " '+'
            if lv_st = 'P'.
              lv_lno += 1.
              lv_rows = lv_rows &&
                |<tr style="background:#ffffff">| &&
                |<td class="ln">{ lv_lno }</td>| &&
                |<td class="cd" style="background:#ffffff{ lv_cmt_b }">{ lt_inline_html[ lv_rb ] }</td></tr>|.
            elseif lv_st = 'C'.
              " skip
            else.
              lv_lno += 1.
              data(lv_il) = ls_bo-text.
              replace all occurrences of `&` in lv_il with `&amp;`.
              replace all occurrences of `<` in lv_il with `&lt;`.
              replace all occurrences of `>` in lv_il with `&gt;`.
              lv_rows = lv_rows &&
                |<tr style="background:#eaffea">| &&
                |<td class="ln" style="color:#006600">{ lv_lno }</td>| &&
                |<td class="cd" style="color:#006600{ lv_cmt_b }">{ lv_il }</td></tr>|.
            endif.
          endif.
          lv_rb += 1.
        endwhile.

        clear lt_dels.
        clear lt_ins.
        lv_pos = lv_scan.
      else.
        lv_pos += 1.
      endif.
    endwhile.

    result =
      |<!DOCTYPE html><html><head><meta charset="utf-8"><style>| &&
      |*\{margin:0;padding:0;box-sizing:border-box\}| &&
      |body\{background:#ffffff;color:#1e1e1e;font:12px/1.5 Consolas,monospace\}| &&
      |.hdr\{background:#f3f3f3;padding:5px 56px;border-bottom:1px solid #ddd;| &&
             |color:#444;font-size:11px;display:flex;gap:8px;| &&
             |justify-content:center;align-items:center;flex-wrap:wrap\}| &&
      |.ttl\{color:#0066aa;font-weight:bold\}| &&
      |.meta\{color:#888\}| &&
      |table\{border-collapse:collapse;width:100%\}| &&
      |.ln\{color:#aaa;text-align:right;padding:1px 10px 1px 5px;| &&
           |user-select:none;min-width:42px;border-right:1px solid #e0e0e0;| &&
           |white-space:nowrap;background:#fafafa\}| &&
      |.cd\{padding:1px 8px;white-space:pre\}| &&
      |</style></head><body>| &&
      |<div class="hdr">| &&
      |<span class="ttl">| && i_title && |</span>| &&
      |<span class="meta">| && i_meta  && |</span>| &&
      |</div>| &&
      |<table><tbody>| && lv_rows &&
      |</tbody></table></body></html>|.
  endmethod.
  method debug_diff_html.
    " Debug rendering: dump diff ops + change blocks + pairing decisions.
    " Mirrors AVEDiff.debugToHtml() in html_simulator/diff.js — same input
    " through both should produce structurally identical output.
    data lv_ops_rows type string.
    data lv_blocks   type string.
    data lv_idx      type i.

    " ── Section 1: raw ops list ──
    lv_idx = 0.
    loop at it_diff into data(ls_op).
      lv_idx += 1.
      data(lv_op_cls) = cond string(
        when ls_op-op = '=' then `eq`
        when ls_op-op = '-' then `del`
        else `ins` ).
      data(lv_text_e) = ls_op-text.
      replace all occurrences of `&` in lv_text_e with `&amp;`.
      replace all occurrences of `<` in lv_text_e with `&lt;`.
      replace all occurrences of `>` in lv_text_e with `&gt;`.
      data(lv_show)   = cond string(
        when lv_text_e is initial then `<em>&lt;empty&gt;</em>`
        else lv_text_e ).
      lv_ops_rows = lv_ops_rows &&
        |<tr class="{ lv_op_cls }"><td class="ln">{ lv_idx }</td>| &&
        |<td class="op">{ ls_op-op }</td><td class="cd">{ lv_show }</td></tr>|.
    endloop.

    " ── Section 2: walk change blocks, record pairing decisions ──
    data lv_pos      type i value 1.
    data lv_total    type i.
    data lv_block_no type i value 0.
    lv_total = lines( it_diff ).

    while lv_pos <= lv_total.
      read table it_diff into data(ls_cur) index lv_pos.
      if ls_cur-op = '='.
        lv_pos += 1.
        continue.
      endif.

      data lt_dels    type string_table.
      data lt_ins     type string_table.
      data lv_bridged type i.
      clear: lt_dels, lt_ins, lv_bridged.
      data lv_scan type i.
      lv_scan = lv_pos.
      while lv_scan <= lv_total.
        read table it_diff into data(ls_s) index lv_scan.
        if ls_s-op = '-'.
          if condense( val = ls_s-text ) <> ``.
            append ls_s-text to lt_dels.
          endif.
          lv_scan += 1.
        elseif ls_s-op = '+'.
          if condense( val = ls_s-text ) <> ``.
            append ls_s-text to lt_ins.
          endif.
          lv_scan += 1.
        elseif ls_s-op = '=' and condense( val = ls_s-text ) = ``.
          " Bridge short empty '=' if more changes follow (max 1 in a row)
          data lv_peek         type i.
          data lv_extra        type i.
          data lv_more_changes type abap_bool.
          lv_peek = lv_scan + 1.
          lv_extra = 0.
          lv_more_changes = abap_false.
          while lv_peek <= lv_total.
            read table it_diff into data(ls_p) index lv_peek.
            if ls_p-op = '-' or ls_p-op = '+'.
              lv_more_changes = abap_true.
              exit.
            elseif ls_p-op = '=' and condense( val = ls_p-text ) = `` and lv_extra < 1.
              lv_extra += 1.
              lv_peek += 1.
              continue.
            else.
              exit.
            endif.
          endwhile.
          if lv_more_changes = abap_true.
            lv_bridged += 1.
            lv_scan += 1.
          else.
            exit.
          endif.
        else.
          exit.
        endif.
      endwhile.

      lv_block_no += 1.
      data(lv_nd) = lines( lt_dels ).
      data(lv_ni) = lines( lt_ins ).
      data(lv_block_end) = lv_scan - 1.

      data lt_pair_dk type standard table of i with default key.
      data lt_pair_ik type standard table of i with default key.
      data lt_d_paired type table of abap_bool with default key.
      data lt_i_paired type table of abap_bool with default key.
      data lt_dp_dbg   type table of i.
      " Must clear all block-local tables — DATA declarations are method-scoped
      " so they accumulate across iterations of this WHILE loop.
      clear: lt_pair_dk, lt_pair_ik, lt_d_paired, lt_i_paired, lt_dp_dbg.
      do lv_nd times. append abap_false to lt_d_paired. enddo.
      do lv_ni times. append abap_false to lt_i_paired. enddo.

      if lv_nd > 0 and lv_ni > 0.
        data(lv_cols_dbg) = lv_ni + 1.
        data(lv_rows_dbg) = lv_nd + 1.
        data(lv_size_dbg) = lv_rows_dbg * lv_cols_dbg.
        do lv_size_dbg times.
          append 0 to lt_dp_dbg.
        enddo.

        data lv_di_dbg type i.
        data lv_ii_dbg type i.
        lv_di_dbg = 1.
        while lv_di_dbg <= lv_nd.
          lv_ii_dbg = 1.
          while lv_ii_dbg <= lv_ni.
            data(lv_cell_dbg) = lv_di_dbg * lv_cols_dbg + lv_ii_dbg + 1.
            data(lv_hcc_dbg) = zcl_ave_popup_diff=>has_common_chars(
              iv_a = lt_dels[ lv_di_dbg ]
              iv_b = lt_ins[ lv_ii_dbg ] ).
            if lv_hcc_dbg = abap_true.
              data(lv_prev_dbg) = ( lv_di_dbg - 1 ) * lv_cols_dbg + ( lv_ii_dbg - 1 ) + 1.
              lt_dp_dbg[ lv_cell_dbg ] = lt_dp_dbg[ lv_prev_dbg ] + 1.
            else.
              data(lv_up_dbg)   = ( lv_di_dbg - 1 ) * lv_cols_dbg + lv_ii_dbg + 1.
              data(lv_left_dbg) = lv_di_dbg * lv_cols_dbg + ( lv_ii_dbg - 1 ) + 1.
              lt_dp_dbg[ lv_cell_dbg ] = cond i(
                when lt_dp_dbg[ lv_up_dbg ] >= lt_dp_dbg[ lv_left_dbg ] then lt_dp_dbg[ lv_up_dbg ]
                else lt_dp_dbg[ lv_left_dbg ] ).
            endif.
            lv_ii_dbg += 1.
          endwhile.
          lv_di_dbg += 1.
        endwhile.

        lv_di_dbg = lv_nd.
        lv_ii_dbg = lv_ni.
        while lv_di_dbg > 0 and lv_ii_dbg > 0.
          if zcl_ave_popup_diff=>has_common_chars( iv_a = lt_dels[ lv_di_dbg ] iv_b = lt_ins[ lv_ii_dbg ] ) = abap_true.
            insert lv_di_dbg into lt_pair_dk index 1.
            insert lv_ii_dbg into lt_pair_ik index 1.
            lv_di_dbg -= 1.
            lv_ii_dbg -= 1.
          else.
            data(lv_up_bt_dbg)   = ( lv_di_dbg - 1 ) * lv_cols_dbg + lv_ii_dbg + 1.
            data(lv_left_bt_dbg) = lv_di_dbg * lv_cols_dbg + ( lv_ii_dbg - 1 ) + 1.
            if lt_dp_dbg[ lv_up_bt_dbg ] >= lt_dp_dbg[ lv_left_bt_dbg ].
              lv_di_dbg -= 1.
            else.
              lv_ii_dbg -= 1.
            endif.
          endif.
        endwhile.
      endif.

      data lv_pair_rows type string.
      clear lv_pair_rows.
      data lv_k type i.
      lv_k = 1.
      while lv_k <= lines( lt_pair_dk ).
        data(lv_dk) = lt_pair_dk[ lv_k ].
        data(lv_ik) = lt_pair_ik[ lv_k ].
        lt_d_paired[ lv_dk ] = abap_true.
        lt_i_paired[ lv_ik ] = abap_true.

        data(lv_a) = lt_dels[ lv_dk ].
        data(lv_b) = lt_ins[ lv_ik ].

        data(lv_a_e) = lv_a.
        replace all occurrences of `&` in lv_a_e with `&amp;`.
        replace all occurrences of `<` in lv_a_e with `&lt;`.
        replace all occurrences of `>` in lv_a_e with `&gt;`.
        data(lv_b_e) = lv_b.
        replace all occurrences of `&` in lv_b_e with `&amp;`.
        replace all occurrences of `<` in lv_b_e with `&lt;`.
        replace all occurrences of `>` in lv_b_e with `&gt;`.
        data(lv_a_show) = cond string(
          when lv_a_e is initial then `<em>&lt;empty&gt;</em>` else lv_a_e ).
        data(lv_b_show) = cond string(
          when lv_b_e is initial then `<em>&lt;empty&gt;</em>` else lv_b_e ).
        data(lv_inline) = zcl_ave_popup_diff=>char_diff_html( iv_old = lv_a iv_new = lv_b iv_side = 'B' ).

        " ── pairing metrics ──────────────────────────────────────────────────
        data lv_ta_m type string.
        data lv_tb_m type string.
        lv_ta_m = lv_a. lv_tb_m = lv_b.
        while strlen( lv_ta_m ) > 0 and substring( val = lv_ta_m off = 0 len = 1 ) = ` `.
          lv_ta_m = substring( val = lv_ta_m off = 1 len = strlen( lv_ta_m ) - 1 ).
        endwhile.
        while strlen( lv_ta_m ) > 0 and substring( val = lv_ta_m off = strlen( lv_ta_m ) - 1 len = 1 ) = ` `.
          lv_ta_m = substring( val = lv_ta_m off = 0 len = strlen( lv_ta_m ) - 1 ).
        endwhile.
        while strlen( lv_tb_m ) > 0 and substring( val = lv_tb_m off = 0 len = 1 ) = ` `.
          lv_tb_m = substring( val = lv_tb_m off = 1 len = strlen( lv_tb_m ) - 1 ).
        endwhile.
        while strlen( lv_tb_m ) > 0 and substring( val = lv_tb_m off = strlen( lv_tb_m ) - 1 len = 1 ) = ` `.
          lv_tb_m = substring( val = lv_tb_m off = 0 len = strlen( lv_tb_m ) - 1 ).
        endwhile.
        data(lv_la_m) = strlen( lv_ta_m ).
        data(lv_lb_m) = strlen( lv_tb_m ).
        data lv_cp_m type i value 0.
        while lv_cp_m < lv_la_m and lv_cp_m < lv_lb_m.
          if substring( val = lv_ta_m off = lv_cp_m len = 1 ) = substring( val = lv_tb_m off = lv_cp_m len = 1 ).
            lv_cp_m += 1.
          else.
            exit.
          endif.
        endwhile.
        data lv_cs_m type i value 0.
        data(lv_la_rest_m) = lv_la_m - lv_cp_m.
        data(lv_lb_rest_m) = lv_lb_m - lv_cp_m.
        while lv_cs_m < lv_la_rest_m and lv_cs_m < lv_lb_rest_m.
          if substring( val = lv_ta_m off = lv_la_m - 1 - lv_cs_m len = 1 ) =
             substring( val = lv_tb_m off = lv_lb_m - 1 - lv_cs_m len = 1 ).
            lv_cs_m += 1.
          else.
            exit.
          endif.
        endwhile.
        data lv_mid_am type string.
        data lv_mid_bm type string.
        data(lv_mid_la_m) = lv_la_m - lv_cp_m - lv_cs_m.
        data(lv_mid_lb_m) = lv_lb_m - lv_cp_m - lv_cs_m.
        if lv_mid_la_m > 0. lv_mid_am = substring( val = lv_ta_m off = lv_cp_m len = lv_mid_la_m ). endif.
        if lv_mid_lb_m > 0. lv_mid_bm = substring( val = lv_tb_m off = lv_cp_m len = lv_mid_lb_m ). endif.
        data(lv_runs_m)  = zcl_ave_popup_diff=>count_edit_runs( iv_a = lv_mid_am iv_b = lv_mid_bm ).
        data(lv_min_m)   = nmin( val1 = lv_la_m val2 = lv_lb_m ).
        data(lv_ratio_m) = cond i( when lv_min_m > 0 then lv_cp_m * 100 / lv_min_m else 0 ).

        " Build annotated lines: prefix in blue, middle normal, suffix in green
        data lv_pfx_e type string.
        data lv_sfx_e type string.
        data lv_amid_e type string.
        data lv_bmid_e type string.
        if lv_cp_m > 0.
          lv_pfx_e = substring( val = lv_ta_m off = 0 len = lv_cp_m ).
          replace all occurrences of `&` in lv_pfx_e with `&amp;`.
          replace all occurrences of `<` in lv_pfx_e with `&lt;`.
          replace all occurrences of `>` in lv_pfx_e with `&gt;`.
        endif.
        if lv_cs_m > 0.
          lv_sfx_e = substring( val = lv_ta_m off = lv_la_m - lv_cs_m len = lv_cs_m ).
          replace all occurrences of `&` in lv_sfx_e with `&amp;`.
          replace all occurrences of `<` in lv_sfx_e with `&lt;`.
          replace all occurrences of `>` in lv_sfx_e with `&gt;`.
        endif.
        lv_amid_e = lv_mid_am. lv_bmid_e = lv_mid_bm.
        replace all occurrences of `&` in lv_amid_e with `&amp;`.
        replace all occurrences of `<` in lv_amid_e with `&lt;`.
        replace all occurrences of `>` in lv_amid_e with `&gt;`.
        replace all occurrences of `&` in lv_bmid_e with `&amp;`.
        replace all occurrences of `<` in lv_bmid_e with `&lt;`.
        replace all occurrences of `>` in lv_bmid_e with `&gt;`.
        data(lv_ann_a) = |<span style="color:#0055cc">{ lv_pfx_e }</span>{ lv_amid_e }<span style="color:#006600">{ lv_sfx_e }</span>|.
        data(lv_ann_b) = |<span style="color:#0055cc">{ lv_pfx_e }</span>{ lv_bmid_e }<span style="color:#006600">{ lv_sfx_e }</span>|.
        data(lv_metrics) = |cp={ lv_cp_m } cs={ lv_cs_m } ratio={ lv_ratio_m }% runs={ lv_runs_m }|.

        lv_pair_rows = lv_pair_rows &&
          |<tr><td class="ln">{ lv_dk }/{ lv_ik }</td>| &&
          |<td class="cd"><span class="del-tag">-</span> <code>{ lv_ann_a }</code></td>| &&
          |<td class="cd"><span class="ins-tag">+</span> <code>{ lv_ann_b }</code></td>| &&
          |<td><span class="ok">PAIR</span><br><small style="color:#888">{ lv_metrics }</small></td>| &&
          |<td class="cd">{ lv_inline }</td></tr>|.
        lv_k += 1.
      endwhile.

      data lv_leftover type string.
      clear lv_leftover.
      lv_k = 1.
      while lv_k <= lv_nd.
        if lt_d_paired[ lv_k ] = abap_false.
          data(lv_d_e) = lt_dels[ lv_k ].
          replace all occurrences of `&` in lv_d_e with `&amp;`.
          replace all occurrences of `<` in lv_d_e with `&lt;`.
          replace all occurrences of `>` in lv_d_e with `&gt;`.
          data(lv_d_show) = cond string( when lv_d_e is initial then `<em>&lt;empty&gt;</em>` else lv_d_e ).
          lv_leftover = lv_leftover && |<div class="solo del">SOLO - <code>{ lv_d_show }</code></div>|.
        endif.
        lv_k += 1.
      endwhile.
      lv_k = 1.
      while lv_k <= lv_ni.
        if lt_i_paired[ lv_k ] = abap_false.
          data(lv_i_e) = lt_ins[ lv_k ].
          replace all occurrences of `&` in lv_i_e with `&amp;`.
          replace all occurrences of `<` in lv_i_e with `&lt;`.
          replace all occurrences of `>` in lv_i_e with `&gt;`.
          data(lv_i_show) = cond string( when lv_i_e is initial then `<em>&lt;empty&gt;</em>` else lv_i_e ).
          lv_leftover = lv_leftover && |<div class="solo ins">SOLO + <code>{ lv_i_show }</code></div>|.
        endif.
        lv_k += 1.
      endwhile.

      data(lv_pair_section) = cond string(
        when lv_pair_rows is not initial then
          |<table class="pair"><thead><tr><th>-/+</th><th>del</th><th>ins</th>| &&
          |<th>verdict</th><th>char-diff (if paired)</th></tr></thead>| &&
          |<tbody>| && lv_pair_rows && |</tbody></table>|
        else `<div class="meta">(no del/ins pairs to test)</div>` ).
      data(lv_leftover_section) = cond string(
        when lv_leftover is not initial then |<div class="leftover">{ lv_leftover }</div>|
        else `` ).

      " ── All-combinations matrix (≤8 dels AND ≤8 ins to keep output manageable)
      data lv_matrix_section type string.
      clear lv_matrix_section.
      if lv_nd > 0 and lv_ni > 0 and lv_nd <= 8 and lv_ni <= 8.
        data lv_mx_rows type string.
        clear lv_mx_rows.
        data lv_di_mx type i.
        data lv_ii_mx type i.
        lv_di_mx = 1.
        while lv_di_mx <= lv_nd.
          lv_ii_mx = 1.
          while lv_ii_mx <= lv_ni.
            data(lv_sa) = lt_dels[ lv_di_mx ].
            data(lv_sb) = lt_ins[ lv_ii_mx ].
            data(lv_hcc) = zcl_ave_popup_diff=>has_common_chars( iv_a = lv_sa iv_b = lv_sb ).
            " Trim for metrics
            data lv_ma type string.
            data lv_mb type string.
            lv_ma = lv_sa. lv_mb = lv_sb.
            while strlen( lv_ma ) > 0 and substring( val = lv_ma off = 0 len = 1 ) = ` `.
              lv_ma = substring( val = lv_ma off = 1 len = strlen( lv_ma ) - 1 ).
            endwhile.
            while strlen( lv_ma ) > 0 and substring( val = lv_ma off = strlen( lv_ma ) - 1 len = 1 ) = ` `.
              lv_ma = substring( val = lv_ma off = 0 len = strlen( lv_ma ) - 1 ).
            endwhile.
            while strlen( lv_mb ) > 0 and substring( val = lv_mb off = 0 len = 1 ) = ` `.
              lv_mb = substring( val = lv_mb off = 1 len = strlen( lv_mb ) - 1 ).
            endwhile.
            while strlen( lv_mb ) > 0 and substring( val = lv_mb off = strlen( lv_mb ) - 1 len = 1 ) = ` `.
              lv_mb = substring( val = lv_mb off = 0 len = strlen( lv_mb ) - 1 ).
            endwhile.
            data(lv_la_mx) = strlen( lv_ma ).
            data(lv_lb_mx) = strlen( lv_mb ).
            data lv_cp_mx type i value 0.
            while lv_cp_mx < lv_la_mx and lv_cp_mx < lv_lb_mx.
              if substring( val = lv_ma off = lv_cp_mx len = 1 ) = substring( val = lv_mb off = lv_cp_mx len = 1 ).
                lv_cp_mx += 1.
              else.
                exit.
              endif.
            endwhile.
            data lv_cs_mx type i value 0.
            data(lv_la_rx) = lv_la_mx - lv_cp_mx.
            data(lv_lb_rx) = lv_lb_mx - lv_cp_mx.
            while lv_cs_mx < lv_la_rx and lv_cs_mx < lv_lb_rx.
              if substring( val = lv_ma off = lv_la_mx - 1 - lv_cs_mx len = 1 ) =
                 substring( val = lv_mb off = lv_lb_mx - 1 - lv_cs_mx len = 1 ).
                lv_cs_mx += 1.
              else.
                exit.
              endif.
            endwhile.
            data lv_mid_amx type string.
            data lv_mid_bmx type string.
            data(lv_mla_mx) = lv_la_mx - lv_cp_mx - lv_cs_mx.
            data(lv_mlb_mx) = lv_lb_mx - lv_cp_mx - lv_cs_mx.
            if lv_mla_mx > 0. lv_mid_amx = substring( val = lv_ma off = lv_cp_mx len = lv_mla_mx ). endif.
            if lv_mlb_mx > 0. lv_mid_bmx = substring( val = lv_mb off = lv_cp_mx len = lv_mlb_mx ). endif.
            data(lv_runs_mx)  = zcl_ave_popup_diff=>count_edit_runs( iv_a = lv_mid_amx iv_b = lv_mid_bmx ).
            data(lv_min_mx)   = nmin( val1 = lv_la_mx val2 = lv_lb_mx ).
            data(lv_ratio_mx) = cond i( when lv_min_mx > 0 then lv_cp_mx * 100 / lv_min_mx else 0 ).
            data(lv_verdict)  = cond string( when lv_hcc = abap_true
              then `<span style="color:#006600;font-weight:bold">PAIR</span>`
              else `<span style="color:#cc0000">SKIP</span>` ).
            data(lv_row_bg) = cond string( when lv_hcc = abap_true then `#eaffea` else `#fff8f8` ).
            lv_mx_rows = lv_mx_rows &&
              |<tr style="background:{ lv_row_bg }">| &&
              |<td class="ln">{ lv_di_mx }/{ lv_ii_mx }</td>| &&
              |<td>{ lv_verdict }</td>| &&
              |<td>cp={ lv_cp_mx }&nbsp;cs={ lv_cs_mx }&nbsp;ratio={ lv_ratio_mx }%&nbsp;runs={ lv_runs_mx }</td>| &&
              |</tr>|.
            lv_ii_mx += 1.
          endwhile.
          lv_di_mx += 1.
        endwhile.
        lv_matrix_section =
          |<details style="margin-top:4px"><summary style="cursor:pointer;color:#555;font-size:11px">| &&
          |All { lv_nd }×{ lv_ni } combinations</summary>| &&
          |<table style="width:auto;margin-top:4px"><thead><tr>| &&
          |<th>d/i</th><th>verdict</th><th>metrics</th></tr></thead>| &&
          |<tbody>{ lv_mx_rows }</tbody></table></details>|.
      endif.

      data(lv_bridge_note) = cond string(
        when lv_bridged > 0 then | <span class="meta">— bridged { lv_bridged } empty '=' line(s)</span>|
        else `` ).
      lv_blocks = lv_blocks &&
        |<div class="block"><h3>Block #{ lv_block_no } | &&
        |<span class="meta">({ lv_nd } dels, { lv_ni } ins, ops [{ lv_pos }..{ lv_block_end }])</span>| &&
        lv_bridge_note && |</h3>| &&
        lv_pair_section && lv_leftover_section && lv_matrix_section && |</div>|.

      lv_pos = lv_scan.
    endwhile.

    if lv_blocks is initial.
      lv_blocks = `<div class="meta">(no change blocks)</div>`.
    endif.

    result =
      |<!DOCTYPE html><html><head><meta charset="utf-8"><style>| &&
      |*\{margin:0;padding:0;box-sizing:border-box\}| &&
      |body\{background:#fff;color:#222;font:12px/1.5 Segoe UI,sans-serif;padding:10px\}| &&
      |h2\{font-size:13px;margin:14px 0 6px;color:#0066aa;border-bottom:1px solid #ddd;padding-bottom:3px\}| &&
      |h3\{font-size:12px;margin:8px 0 4px;color:#444\}| &&
      |.hdr\{background:#f3f3f3;padding:6px 10px;border:1px solid #ddd;color:#444;| &&
            |display:flex;gap:14px;flex-wrap:wrap;margin-bottom:8px\}| &&
      |.ttl\{color:#0066aa;font-weight:bold\}.meta\{color:#888;font-weight:normal;font-size:11px\}| &&
      |table\{border-collapse:collapse;width:100%;font:11px/1.4 Consolas,monospace;margin-bottom:6px\}| &&
      |th,td\{padding:2px 6px;border:1px solid #e0e0e0;text-align:left;vertical-align:top\}| &&
      |th\{background:#fafafa;font-weight:600\}| &&
      |.ln\{color:#aaa;text-align:right;width:40px;background:#fafafa\}| &&
      |.op\{width:24px;text-align:center;font-weight:bold\}| &&
      |tr.eq td\{color:#888\}| &&
      |tr.del\{background:#ffecec\}tr.del td.op\{color:#cc0000\}| &&
      |tr.ins\{background:#eaffea\}tr.ins td.op\{color:#006600\}| &&
      |.cd\{white-space:pre;font:11px/1.4 Consolas,monospace\}| &&
      |code\{font:11px/1.4 Consolas,monospace;background:#f7f7f7;padding:1px 4px;border-radius:2px\}| &&
      |.block\{border:1px solid #ddd;padding:6px;margin-bottom:8px;border-radius:3px;background:#fcfcfc\}| &&
      |.pair th\{background:#eef\}| &&
      |.ok\{color:#006600;font-weight:bold\}.bad\{color:#cc0000;font-weight:bold\}| &&
      |.del-tag\{color:#cc0000;font-weight:bold\}.ins-tag\{color:#006600;font-weight:bold\}| &&
      |.solo\{margin:2px 0;padding:2px 6px;border-radius:2px;font:11px/1.4 Consolas,monospace\}| &&
      |.solo.del\{background:#ffecec;color:#cc0000\}| &&
      |.solo.ins\{background:#eaffea;color:#006600\}| &&
      |.leftover\{margin-top:4px\}| &&
      |em\{color:#aaa;font-style:italic\}| &&
      |</style></head><body>| &&
      |<div class="hdr"><span class="ttl">DEBUG: | && i_title && |</span>| &&
      |<span class="meta">| && i_meta && |</span></div>| &&
      |<h2>1. Diff ops ({ lv_total } total)</h2>| &&
      |<table><thead><tr><th>#</th><th>op</th><th>text</th></tr></thead>| &&
      |<tbody>| && lv_ops_rows && |</tbody></table>| &&
      |<h2>2. Change blocks &amp; pairing decisions</h2>| && lv_blocks &&
      |</body></html>|.
  endmethod.
  method cds_source_to_html.
    " Helper: apply span tags by match positions (avoids regex backreference issues).
    " Processes matches from left to right and wraps each with <span class=css_class>.
    data: lv_rows type string,
          lv_lno  type i.

    data(lv_kw_regex) =
      '\b(define|view|entity|root|as|select|from|key|association|' &&
      'to|one|many|redirected|composition|join|left|outer|inner|cross|on|' &&
      'where|group|by|having|union|all|intersect|except|distinct|order|' &&
      'asc|desc|case|when|then|else|end|and|or|not|null|is|with|' &&
      'parameters|cast|coalesce|concat|upper|lower|substring|length|trim|' &&
      'projection|extend|abstract|transactional|query|interface|' &&
      'draft|enabled|annotate|aspect|type|of|in|between|like|exists|' &&
      'count|sum|avg|min|max|currency|unit|localized|literal|parent|' &&
      'provider|contract|strict|authorization|check)\b'.

    loop at it_source into data(ls_src).
      lv_lno += 1.
      data(lv_line) = conv string( ls_src ).

      replace all occurrences of `&` in lv_line with `&amp;`.
      replace all occurrences of `<` in lv_line with `&lt;`.
      replace all occurrences of `>` in lv_line with `&gt;`.

      data(lv_trimmed) = condense( val = lv_line ).
      data(lv_tlen)    = strlen( lv_trimmed ).

      data lv_cell type string.

      if lv_tlen >= 2 and lv_trimmed(2) = '//'.
        lv_cell = |<span class="cmt">{ lv_line }</span>|.
      elseif lv_tlen >= 2 and lv_trimmed(2) = '/*'.
        lv_cell = |<span class="cmt">{ lv_line }</span>|.
      else.
        lv_cell = lv_line.

        " Highlight @Annotation names using position-based approach.
        data lt_ann type match_result_tab.
        find all occurrences of regex '@[\w.]+'
          in lv_cell results lt_ann ignoring case.
        if lt_ann is not initial.
          data: lv_ann_out type string,
                lv_ann_pos type i.
          data: lv_ann_before type i,
                lv_ann_len    type i,
                lv_ann_off    type i.
          loop at lt_ann into data(ls_ann).
            lv_ann_off    = ls_ann-offset.
            lv_ann_before = lv_ann_off - lv_ann_pos.
            lv_ann_len    = ls_ann-length.
            lv_ann_out = lv_ann_out &&
              lv_cell+lv_ann_pos(lv_ann_before) &&
              |<span class="ann">{ lv_cell+lv_ann_off(lv_ann_len) }</span>|.
            lv_ann_pos = lv_ann_off + lv_ann_len.
          endloop.
          lv_cell = lv_ann_out && lv_cell+lv_ann_pos.
        endif.

        " Highlight CDS keywords using position-based approach.
        data lt_kw type match_result_tab.
        find all occurrences of regex lv_kw_regex
          in lv_cell results lt_kw ignoring case.
        if lt_kw is not initial.
          data: lv_kw_out type string,
                lv_kw_pos type i.
          data: lv_kw_before type i,
                lv_kw_len    type i,
                lv_kw_off    type i.
          loop at lt_kw into data(ls_kw).
            lv_kw_off    = ls_kw-offset.
            lv_kw_before = lv_kw_off - lv_kw_pos.
            lv_kw_len    = ls_kw-length.
            lv_kw_out = lv_kw_out &&
              lv_cell+lv_kw_pos(lv_kw_before) &&
              |<span class="kw">{ lv_cell+lv_kw_off(lv_kw_len) }</span>|.
            lv_kw_pos = lv_kw_off + lv_kw_len.
          endloop.
          lv_cell = lv_kw_out && lv_cell+lv_kw_pos.
        endif.
      endif.

      lv_rows = lv_rows &&
        |<tr><td class="ln">{ lv_lno }</td>| &&
        |<td class="cd">{ lv_cell }</td></tr>|.
    endloop.

    rv_html =
      |<!DOCTYPE html><html><head><meta charset="utf-8"><style>| &&
      |*\{margin:0;padding:0;box-sizing:border-box\}| &&
      |body\{background:#fff;color:#1e1e1e;font:12px/1.5 Consolas,monospace\}| &&
      |.hdr\{background:#f3f3f3;padding:5px 12px;border-bottom:1px solid #ddd;| &&
             |color:#444;font-size:11px;display:flex;gap:16px;flex-wrap:wrap\}| &&
      |.ttl\{color:#0066aa;font-weight:bold\}| &&
      |.meta\{color:#888\}| &&
      |table\{border-collapse:collapse;width:100%\}| &&
      |tr:hover td\{background:#f0f4fa\}| &&
      |.ln\{color:#aaa;text-align:right;padding:1px 10px 1px 5px;| &&
           |user-select:none;min-width:42px;border-right:1px solid #e0e0e0;| &&
           |white-space:nowrap;background:#fafafa\}| &&
      |.cd\{padding:1px 8px;white-space:pre\}| &&
      |.kw\{color:#0070c1;font-weight:bold\}| &&
      |.ann\{color:#267f99\}| &&
      |.cmt\{color:#008000\}| &&
      |</style></head><body>| &&
      |<div class="hdr">| &&
      |<span class="ttl">| && i_title && |</span>| &&
      |<span class="meta">| && i_meta  && |</span>| &&
      |</div>| &&
      |<table><tbody>| && lv_rows &&
      |</tbody></table></body></html>|.
  endmethod.

endclass.

class zcl_ave_popup_diff implementation.
  method compute_diff.
    data(lv_nold) = lines( it_old ).
    data(lv_nnew) = lines( it_new ).

    " Build comparison keys — uppercase when ignore_case, otherwise verbatim
    data lt_old_key type string_table.
    data lt_new_key type string_table.
    loop at it_old into data(ls_oi).
      append cond string( when i_ignore_case = abap_true
        then to_upper( conv string( ls_oi ) )
        else conv string( ls_oi ) ) to lt_old_key.
    endloop.
    loop at it_new into data(ls_ni).
      append cond string( when i_ignore_case = abap_true
        then to_upper( conv string( ls_ni ) )
        else conv string( ls_ni ) ) to lt_new_key.
    endloop.

    " Simplest possible diff for large files: two-pointer walk with a
    " short look-ahead window for resync. No hash maps, no DP matrix —
    " just the result table in memory. Handles "one line deleted, rest
    " identical" correctly (resync at k=1). Degrades to 1:1 substitution
    " if no match within lc_window steps.
    if lv_nold > 10000 or lv_nnew > 10000.
      constants lc_window type i value 50.
      data(lo_p) = new zcl_ave_progress(
        i_title          = i_title
        i_threshold_secs = 15
        i_confirm_key    = cond string(
             when i_confirm_key is not initial then conv string( i_confirm_key )
             else conv string( i_title ) ) ).
      data lv_i1  type i value 1.
      data lv_j1  type i value 1.
      data lv_tot type i.
      lv_tot = lv_nold + lv_nnew.

      while lv_i1 <= lv_nold or lv_j1 <= lv_nnew.
        if lo_p->check( i_remaining = lv_tot - lv_i1 - lv_j1 + 2
                        i_total     = lv_tot ) = abap_true.
          return.
        endif.
        if lv_i1 > lv_nold.
          append value ty_diff_op( op = '+' text = conv string( it_new[ lv_j1 ] ) ) to result.
          lv_j1 += 1.
          continue.
        endif.
        if lv_j1 > lv_nnew.
          append value ty_diff_op( op = '-' text = conv string( it_old[ lv_i1 ] ) ) to result.
          lv_i1 += 1.
          continue.
        endif.
        if lt_old_key[ lv_i1 ] = lt_new_key[ lv_j1 ].
          append value ty_diff_op( op = '=' text = conv string( it_new[ lv_j1 ] ) ) to result.
          lv_i1 += 1.
          lv_j1 += 1.
          continue.
        endif.

        " Mismatch — probe forward up to lc_window steps to find resync.
        data lv_k    type i.
        data lv_mode type c.
        clear lv_mode.
        lv_k = 1.
        while lv_k <= lc_window.
          " old[i] appears at new[j+k]? → k inserts
          if lv_j1 + lv_k <= lv_nnew and lt_new_key[ lv_j1 + lv_k ] = lt_old_key[ lv_i1 ].
            lv_mode = '+'.
            exit.
          endif.
          " new[j] appears at old[i+k]? → k deletes
          if lv_i1 + lv_k <= lv_nold and lt_old_key[ lv_i1 + lv_k ] = lt_new_key[ lv_j1 ].
            lv_mode = '-'.
            exit.
          endif.
          lv_k += 1.
        endwhile.

        if lv_mode = '+'.
          do lv_k times.
            append value ty_diff_op( op = '+' text = conv string( it_new[ lv_j1 ] ) ) to result.
            lv_j1 += 1.
          enddo.
        elseif lv_mode = '-'.
          do lv_k times.
            append value ty_diff_op( op = '-' text = conv string( it_old[ lv_i1 ] ) ) to result.
            lv_i1 += 1.
          enddo.
        else.
          " No match within window — substitute 1:1 and advance both sides.
          append value ty_diff_op( op = '-' text = conv string( it_old[ lv_i1 ] ) ) to result.
          append value ty_diff_op( op = '+' text = conv string( it_new[ lv_j1 ] ) ) to result.
          lv_i1 += 1.
          lv_j1 += 1.
        endif.
      endwhile.
      return.
    endif.

    " Build flat 2D DP table: (lv_nold+1) x (lv_nnew+1)
    data(lv_cols) = lv_nnew + 1.
    data(lv_rows) = lv_nold + 1.
    data lt_dp type table of i.
    data(lv_size) = lv_rows * lv_cols.
    do lv_size times.
      append 0 to lt_dp.
    enddo.

    " Fill DP
    data(lo_progress) = new zcl_ave_progress(
      i_title          = i_title
      i_threshold_secs = 15
      i_confirm_key    = cond string(
           when i_confirm_key is not initial then conv string( i_confirm_key )
           else conv string( i_title ) ) ).
    data lv_i type i.
    data lv_j type i.
    lv_i = 1.
    loop at lt_old_key into data(ls_old).
      if lo_progress->check(
           i_remaining = lv_nold - lv_i + 1
           i_total     = lv_nold ) = abap_true.
        return.
      endif.
      lv_j = 1.
      loop at lt_new_key into data(ls_new).
        data(lv_cell) = lv_i * lv_cols + lv_j + 1.
        if ls_old = ls_new.
          data(lv_prev) = ( lv_i - 1 ) * lv_cols + ( lv_j - 1 ) + 1.
          lt_dp[ lv_cell ] = lt_dp[ lv_prev ] + 1.
        else.
          data(lv_up)   = ( lv_i - 1 ) * lv_cols + lv_j + 1.
          data(lv_left) = lv_i * lv_cols + ( lv_j - 1 ) + 1.
          data(lv_vup)   = lt_dp[ lv_up ].
          data(lv_vleft) = lt_dp[ lv_left ].
          lt_dp[ lv_cell ] = cond i( when lv_vup >= lv_vleft then lv_vup else lv_vleft ).
        endif.
        lv_j += 1.
      endloop.
      lv_i += 1.
    endloop.

    " Backtrack to build diff ops (prepend into result).
    " Prefer deletion over insertion (cup > cleft) so '-' precedes '+'
    " in the same change block – keeps related pairs together.
    lv_i = lv_nold.
    lv_j = lv_nnew.
    while lv_i > 0 or lv_j > 0.
      if lv_i > 0 and lv_j > 0.
        read table it_old into data(ls_bo) index lv_i.
        read table it_new into data(ls_bn) index lv_j.
        if lt_old_key[ lv_i ] = lt_new_key[ lv_j ].
          insert value ty_diff_op( op = '=' text = conv string( ls_bn ) ) into result index 1.
          lv_i -= 1.
          lv_j -= 1.
        else.
          data(lv_cup)   = ( lv_i - 1 ) * lv_cols + lv_j + 1.
          data(lv_cleft) = lv_i * lv_cols + ( lv_j - 1 ) + 1.
          if lt_dp[ lv_cup ] >= lt_dp[ lv_cleft ].
            insert value ty_diff_op( op = '-' text = conv string( ls_bo ) ) into result index 1.
            lv_i -= 1.
          else.
            insert value ty_diff_op( op = '+' text = conv string( ls_bn ) ) into result index 1.
            lv_j -= 1.
          endif.
        endif.
      elseif lv_i > 0.
        read table it_old into data(ls_bo2) index lv_i.
        insert value ty_diff_op( op = '-' text = conv string( ls_bo2 ) ) into result index 1.
        lv_i -= 1.
      else.
        read table it_new into data(ls_bn2) index lv_j.
        insert value ty_diff_op( op = '+' text = conv string( ls_bn2 ) ) into result index 1.
        lv_j -= 1.
      endif.
    endwhile.
  endmethod.
  method char_diff_html.
    " Build char-level LCS ops and render grouped spans.
    data lv_old_t type string.
    data lv_new_t type string.
    lv_old_t = iv_old.
    lv_new_t = iv_new.
    while strlen( lv_old_t ) > 0 and substring( val = lv_old_t off = strlen( lv_old_t ) - 1 len = 1 ) = ` `.
      lv_old_t = substring( val = lv_old_t off = 0 len = strlen( lv_old_t ) - 1 ).
    endwhile.
    while strlen( lv_new_t ) > 0 and substring( val = lv_new_t off = strlen( lv_new_t ) - 1 len = 1 ) = ` `.
      lv_new_t = substring( val = lv_new_t off = 0 len = strlen( lv_new_t ) - 1 ).
    endwhile.

    data(lv_lo) = strlen( lv_old_t ).
    data(lv_ln) = strlen( lv_new_t ).
    data(lv_cols) = lv_ln + 1.
    data(lv_rows) = lv_lo + 1.

    " Build comparison strings: uppercase when ignore_case, verbatim otherwise.
    " Used for LCS matching only; lv_old_t / lv_new_t still hold original text for rendering.
    data lv_old_cmp type string.
    data lv_new_cmp type string.
    if iv_ignore_case = abap_true.
      lv_old_cmp = to_upper( lv_old_t ).
      lv_new_cmp = to_upper( lv_new_t ).
    else.
      lv_old_cmp = lv_old_t.
      lv_new_cmp = lv_new_t.
    endif.

    data lt_dp type table of i.
    data(lv_size) = lv_rows * lv_cols.
    do lv_size times.
      append 0 to lt_dp.
    enddo.

    data lv_i type i.
    data lv_j type i.
    lv_i = 1.
    while lv_i <= lv_lo.
      lv_j = 1.
      while lv_j <= lv_ln.
        data(lv_cell) = lv_i * lv_cols + lv_j + 1.
        data(lv_off_o) = lv_i - 1.
        data(lv_off_n) = lv_j - 1.
        if lv_old_cmp+lv_off_o(1) = lv_new_cmp+lv_off_n(1).
          data(lv_prev) = ( lv_i - 1 ) * lv_cols + ( lv_j - 1 ) + 1.
          lt_dp[ lv_cell ] = lt_dp[ lv_prev ] + 1.
        else.
          data(lv_up)   = ( lv_i - 1 ) * lv_cols + lv_j + 1.
          data(lv_left) = lv_i * lv_cols + ( lv_j - 1 ) + 1.
          lt_dp[ lv_cell ] = cond i(
            when lt_dp[ lv_up ] >= lt_dp[ lv_left ] then lt_dp[ lv_up ]
            else lt_dp[ lv_left ] ).
        endif.
        lv_j += 1.
      endwhile.
      lv_i += 1.
    endwhile.

    data lt_ops type ty_t_diff.
    lv_i = lv_lo.
    lv_j = lv_ln.
    while lv_i > 0 or lv_j > 0.
      data(lv_off_bo) = lv_i - 1.
      data(lv_off_bn) = lv_j - 1.
      if lv_i > 0 and lv_j > 0 and lv_old_cmp+lv_off_bo(1) = lv_new_cmp+lv_off_bn(1).
        insert value ty_diff_op( op = '=' text = lv_old_t+lv_off_bo(1) ) into lt_ops index 1.
        lv_i -= 1.
        lv_j -= 1.
      elseif lv_j > 0.
        if lv_i = 0.
          insert value ty_diff_op( op = '+' text = lv_new_t+lv_off_bn(1) ) into lt_ops index 1.
          lv_j -= 1.
        elseif lt_dp[ lv_i * lv_cols + ( lv_j - 1 ) + 1 ] > lt_dp[ ( lv_i - 1 ) * lv_cols + lv_j + 1 ].
          insert value ty_diff_op( op = '+' text = lv_new_t+lv_off_bn(1) ) into lt_ops index 1.
          lv_j -= 1.
        elseif lv_i > 0.
          insert value ty_diff_op( op = '-' text = lv_old_t+lv_off_bo(1) ) into lt_ops index 1.
          lv_i -= 1.
        endif.
      elseif lv_i > 0.
        insert value ty_diff_op( op = '-' text = lv_old_t+lv_off_bo(1) ) into lt_ops index 1.
        lv_i -= 1.
      endif.
    endwhile.

    collapse_token_ops( changing ct_ops = lt_ops ).

    data(lv_del_style) = `background:#ffb3b3;color:#cc0000;padding:0 2px;outline:1px solid #c66`.
    data(lv_ins_style) = `background:#afffaf;color:#006600;padding:0 2px;outline:1px solid #6c6`.
    data lv_buf    type string.
    data lv_buf_op type c length 1.

    loop at lt_ops into data(ls_part).
      if lv_buf_op is initial or ls_part-op = lv_buf_op.
        lv_buf = lv_buf && ls_part-text.
        lv_buf_op = ls_part-op.
        continue.
      endif.

      data(lv_emit) = lv_buf.
      replace all occurrences of `&` in lv_emit with `&amp;`.
      replace all occurrences of `<` in lv_emit with `&lt;`.
      replace all occurrences of `>` in lv_emit with `&gt;`.
      case lv_buf_op.
        when '='.
          result = result && lv_emit.
        when '-'.
          if iv_side <> 'N'.
            data(lv_emit_cnd) = lv_emit.
            condense lv_emit_cnd.
            if lv_emit_cnd is not initial.   " skip pure-space deletions (alignment gaps)
              replace all occurrences of ` ` in lv_emit with `&nbsp;`.
              result = result && |<span style="{ lv_del_style }">{ lv_emit }</span>|.
            endif.
          endif.
        when '+'.
          if iv_side <> 'O'.
            replace all occurrences of ` ` in lv_emit with `&nbsp;`.
            result = result && |<span style="{ lv_ins_style }">{ lv_emit }</span>|.
          endif.
      endcase.

      lv_buf = ls_part-text.
      lv_buf_op = ls_part-op.
    endloop.

    if lv_buf is not initial.
      data(lv_emit_last) = lv_buf.
      replace all occurrences of `&` in lv_emit_last with `&amp;`.
      replace all occurrences of `<` in lv_emit_last with `&lt;`.
      replace all occurrences of `>` in lv_emit_last with `&gt;`.
      case lv_buf_op.
        when '='.
          result = result && lv_emit_last.
        when '-'.
          if iv_side <> 'N'.
            data(lv_emit_last_cnd) = lv_emit_last.
            condense lv_emit_last_cnd.
            if lv_emit_last_cnd is not initial.  " skip pure-space deletions
              replace all occurrences of ` ` in lv_emit_last with `&nbsp;`.
              result = result && |<span style="{ lv_del_style }">{ lv_emit_last }</span>|.
            endif.
          endif.
        when '+'.
          if iv_side <> 'O'.
            replace all occurrences of ` ` in lv_emit_last with `&nbsp;`.
            result = result && |<span style="{ lv_ins_style }">{ lv_emit_last }</span>|.
          endif.
      endcase.
    endif.
  endmethod.
  method has_common_chars.
    " Mirrors hasCommonChars() in html_simulator/diff.js.
    data lv_a type string.
    data lv_b type string.
    lv_a = iv_a.
    lv_b = iv_b.

    while strlen( lv_a ) > 0 and substring( val = lv_a off = 0 len = 1 ) = ` `.
      lv_a = substring( val = lv_a off = 1 len = strlen( lv_a ) - 1 ).
    endwhile.
    while strlen( lv_b ) > 0 and substring( val = lv_b off = 0 len = 1 ) = ` `.
      lv_b = substring( val = lv_b off = 1 len = strlen( lv_b ) - 1 ).
    endwhile.
    while strlen( lv_a ) > 0 and substring( val = lv_a off = strlen( lv_a ) - 1 len = 1 ) = ` `.
      lv_a = substring( val = lv_a off = 0 len = strlen( lv_a ) - 1 ).
    endwhile.
    while strlen( lv_b ) > 0 and substring( val = lv_b off = strlen( lv_b ) - 1 len = 1 ) = ` `.
      lv_b = substring( val = lv_b off = 0 len = strlen( lv_b ) - 1 ).
    endwhile.

    data(lv_la) = strlen( lv_a ).
    data(lv_lb) = strlen( lv_b ).
    if lv_la = 0 or lv_lb = 0.
      result = abap_true.
      return.
    endif.
    if lv_a = lv_b.
      result = abap_true.
      return.
    endif.

    data lv_shorter type string.
    data lv_longer  type string.
    if lv_la < lv_lb.
      lv_shorter = lv_a.
      lv_longer  = lv_b.
    else.
      lv_shorter = lv_b.
      lv_longer  = lv_a.
    endif.

    data(lv_shifted) = cond string(
      when strlen( lv_longer ) > 1 then substring( val = lv_longer off = 1 )
      else `` ).
    if lv_shifted = lv_shorter.
      result = abap_true.
      return.
    endif.

    data(lv_tail) = lv_shifted.
    while strlen( lv_tail ) > 0 and lv_tail(1) = ` `.
      lv_tail = substring( val = lv_tail off = 1 len = strlen( lv_tail ) - 1 ).
    endwhile.
    if lv_tail = lv_shorter.
      result = abap_true.
      return.
    endif.

    " One line's content is contained in the other
    " (e.g. commented-out: old="  email TYPE x," new="  "email TYPE x, "comment")
    if strlen( lv_shorter ) >= 3 and lv_longer cs lv_shorter.
      result = abap_true.
      return.
    endif.

    data lv_cp type i value 0.
    while lv_cp < lv_la and lv_cp < lv_lb.
      if substring( val = lv_a off = lv_cp len = 1 ) =
         substring( val = lv_b off = lv_cp len = 1 ).
        lv_cp += 1.
      else.
        exit.
      endif.
    endwhile.
    if lv_cp < 3. result = abap_false. return. endif.

    " Prefix must cover ≥25% of the shorter line — prevents pairing lines that
    " share only a leading keyword (OR, AND, IF, ...) but differ in substance.
    data(lv_min_len) = nmin( val1 = lv_la val2 = lv_lb ).
    if lv_cp * 4 < lv_min_len. result = abap_false. return. endif.

    " Strip common suffix to isolate the changed middle
    data lv_cs      type i value 0.
    data lv_la_rest type i.
    data lv_lb_rest type i.
    lv_la_rest = lv_la - lv_cp.
    lv_lb_rest = lv_lb - lv_cp.
    while lv_cs < lv_la_rest and lv_cs < lv_lb_rest.
      if substring( val = lv_a off = lv_la - 1 - lv_cs len = 1 ) =
         substring( val = lv_b off = lv_lb - 1 - lv_cs len = 1 ).
        lv_cs += 1.
      else.
        exit.
      endif.
    endwhile.
    data lv_mid_a  type string.
    data lv_mid_b  type string.
    data lv_mid_la type i.
    data lv_mid_lb type i.
    lv_mid_la = lv_la - lv_cp - lv_cs.
    lv_mid_lb = lv_lb - lv_cp - lv_cs.
    if lv_mid_la > 0.
      lv_mid_a = substring( val = lv_a off = lv_cp len = lv_mid_la ).
    endif.
    if lv_mid_lb > 0.
      lv_mid_b = substring( val = lv_b off = lv_cp len = lv_mid_lb ).
    endif.
    " More than 2 edit runs in the middle → lines differ in too many places to pair
    if count_edit_runs( iv_a = lv_mid_a iv_b = lv_mid_b ) > 2.
      result = abap_false. return.
    endif.
    result = abap_true.
  endmethod.
  method build_blame_map.
    " Filter versions for this object within [i_from, i_to] and order ascending
    data lt_vers type zif_ave_popup_types=>ty_t_version_row.
    if i_from is initial.
      " New object — all lines credited to the object version author
      loop at it_versions into data(ls_v)
        where versno  <= i_to
          and objtype  = i_objtype
          and objname  = i_objname.
        append ls_v to lt_vers.
      endloop.
    else.
      " Existing object — trace changes across versions
      loop at it_versions into ls_v
        where versno  >= i_from
          and versno  <= i_to
          and objtype  = i_objtype
          and objname  = i_objname.
        append ls_v to lt_vers.
      endloop.
    endif.
    sort lt_vers by versno ascending datum ascending zeit ascending.

    if lt_vers is initial. return. endif.

    data lt_prev_src type abaptxt255_tab.
    data lt_cur_src type abaptxt255_tab.
    data(ls_first) = lt_vers[ 1 ].
    lt_prev_src = zcl_ave_popup_data=>get_ver_source(
      i_objtype = ls_first-objtype
      i_objname = ls_first-objname
      i_versno  = ls_first-versno
      i_korrnum = ls_first-korrnum
      i_author  = ls_first-author
      i_datum   = ls_first-datum
      i_zeit    = ls_first-zeit ).

    if i_from is initial.
      loop at lt_prev_src into data(ls_line).
        append value zif_ave_popup_types=>ty_blame_entry(
          text        = conv string( ls_line )
          author      = cond #( when ls_first-obj_owner is not initial then ls_first-obj_owner else ls_first-author )
          author_name = cond #( when ls_first-obj_owner is not initial then ls_first-obj_owner_name else ls_first-author_name )
          datum       = ls_first-datum
          zeit        = ls_first-zeit
          versno_text = ls_first-versno_text
          korrnum     = ls_first-korrnum
          task        = ls_first-task
          task_text   = ls_first-korr_text
        ) to result.
      endloop.
    elseif lines( lt_vers ) < 2.
      return.
    endif.

    if lines( lt_vers ) < 2. return. endif.

    data(lv_total) = lines( lt_vers ) - 1.
    data lv_idx type i value 2.
    while lv_idx <= lines( lt_vers ).
      data(lv_step) = lv_idx - 1.
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = conv i( lv_step * 100 / lv_total )
          text       = conv char70( |Computing blame ({ lv_step }/{ lv_total })| ).
      data(ls_ver) = lt_vers[ lv_idx ].
      lt_cur_src = zcl_ave_popup_data=>get_ver_source(
        i_objtype = ls_ver-objtype
        i_objname = ls_ver-objname
        i_versno  = ls_ver-versno
        i_korrnum = ls_ver-korrnum
        i_author  = ls_ver-author
        i_datum   = ls_ver-datum
        i_zeit    = ls_ver-zeit ).
      data(lt_diff) = compute_diff(
        it_old        = lt_prev_src
        it_new        = lt_cur_src
        i_title       = |Computing blame ({ lv_step }/{ lv_total })|
        i_confirm_key = |BLAME~{ i_objtype }~{ i_objname }| ).

      loop at lt_diff into data(ls_d).
        if ls_d-op = '+'.
          data(lv_text) = ls_d-text.
          delete result where text = lv_text.
          append value zif_ave_popup_types=>ty_blame_entry(
            text        = lv_text
            author      = cond #( when ls_ver-obj_owner is not initial then ls_ver-obj_owner else ls_ver-author )
            author_name = cond #( when ls_ver-obj_owner is not initial then ls_ver-obj_owner_name else ls_ver-author_name )
            datum       = ls_ver-datum
            zeit        = ls_ver-zeit
            versno_text = ls_ver-versno_text
            korrnum     = ls_ver-korrnum
            task        = ls_ver-task
            task_text   = ls_ver-korr_text
          ) to result.
        elseif ls_d-op = '-'.
          delete et_blame_deleted where text = ls_d-text.
          append value zif_ave_popup_types=>ty_blame_entry(
            text        = ls_d-text
            author      = cond #( when ls_ver-obj_owner is not initial then ls_ver-obj_owner else ls_ver-author )
            author_name = cond #( when ls_ver-obj_owner is not initial then ls_ver-obj_owner_name else ls_ver-author_name )
            datum       = ls_ver-datum
            zeit        = ls_ver-zeit
            versno_text = ls_ver-versno_text
            korrnum     = ls_ver-korrnum
            task        = ls_ver-task
            task_text   = ls_ver-korr_text
          ) to et_blame_deleted.
          delete result where text = ls_d-text.
        endif.
      endloop.

      lt_prev_src = lt_cur_src.
      lv_idx += 1.
    endwhile.
  endmethod.
  method count_edit_runs.
    " Tokenize by spaces; keep non-empty tokens (single-char tokens like = ( ) are valid anchors)
    data lt_a       type table of string.
    data lt_b       type table of string.
    data lt_tmp     type table of string.
    data lt_pair_ia type table of i.   " greedy-matched indices in lt_a (1-based)
    data lt_pair_ib type table of i.   " greedy-matched indices in lt_b (1-based)
    data lv_jstart  type i.
    data lv_jb      type i.
    data lv_ia      type i.
    data lv_np      type i.
    data lv_k       type i.
    data lv_pia     type i.
    data lv_pib     type i.
    data lv_pia2    type i.
    data lv_pib2    type i.

    split iv_a at ` ` into table lt_a.
    split iv_b at ` ` into table lt_b.
    loop at lt_a into data(lv_t). if lv_t is not initial. append lv_t to lt_tmp. endif. endloop.
    lt_a = lt_tmp. clear lt_tmp.
    loop at lt_b into lv_t. if lv_t is not initial. append lv_t to lt_tmp. endif. endloop.
    lt_b = lt_tmp.

    data(lv_na) = lines( lt_a ).
    data(lv_nb) = lines( lt_b ).
    if lv_na = 0 and lv_nb = 0. return.         endif.
    if lv_na = 0 or  lv_nb = 0. result = 1. return. endif.

    " Greedy forward scan: find matching token pairs (ia, ib) in ascending order
    lv_jstart = 1.
    do lv_na times.
      lv_ia = sy-index.
      lv_jb = lv_jstart.
      while lv_jb <= lv_nb.
        if lt_a[ lv_ia ] = lt_b[ lv_jb ].
          append lv_ia to lt_pair_ia.
          append lv_jb to lt_pair_ib.
          lv_jstart = lv_jb + 1.
          exit.
        endif.
        lv_jb += 1.
      endwhile.
    enddo.

    lv_np = lines( lt_pair_ia ).
    if lv_np = 0. result = 1. return. endif.

    " Count edit runs: unmatched region before first island,
    " between consecutive islands, and after last island
    lv_pia = lt_pair_ia[ 1 ].
    lv_pib = lt_pair_ib[ 1 ].
    if lv_pia > 1 or lv_pib > 1. result += 1. endif.
    do lv_np - 1 times.
      lv_k    = sy-index.
      lv_pia  = lt_pair_ia[ lv_k ].
      lv_pib  = lt_pair_ib[ lv_k ].
      lv_pia2 = lt_pair_ia[ lv_k + 1 ].
      lv_pib2 = lt_pair_ib[ lv_k + 1 ].
      if lv_pia2 > lv_pia + 1 or lv_pib2 > lv_pib + 1.
        result += 1.
      endif.
    enddo.
    lv_pia = lt_pair_ia[ lv_np ].
    lv_pib = lt_pair_ib[ lv_np ].
    if lv_pia < lv_na or lv_pib < lv_nb. result += 1. endif.
  endmethod.
  method collapse_token_ops.
    " Collapse word tokens where both deletions AND insertions exist (>2 total)
    " into whole-token replace, rather than showing partial char-level matches.
    data lt_result type ty_t_diff.
    data lv_ts     type i value 1.
    data lv_te     type i.
    data lv_tk     type i.
    data lv_c0     type string.
    data lv_cn     type string.
    data lv_iw     type abap_bool.
    data lv_iwn    type abap_bool.
    data lv_opn    type c length 1.
    data lv_dc     type i.
    data lv_ic     type i.
    data lv_ot     type string.
    data lv_nt     type string.
    data lv_opk    type c length 1.
    data lv_ec     type string.
    data lv_wch    type string value
      'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'.
    data(lv_no) = lines( ct_ops ).
    while lv_ts <= lv_no.
      lv_c0 = ct_ops[ lv_ts ]-text.
      lv_iw = xsdbool( lv_c0 co lv_wch ).
      if lv_iw = abap_false and ct_ops[ lv_ts ]-op = '='.
        append ct_ops[ lv_ts ] to lt_result.
        lv_ts += 1.
        continue.
      endif.
      lv_te = lv_ts.
      while lv_te < lv_no.
        lv_cn  = ct_ops[ lv_te + 1 ]-text.
        lv_iwn = xsdbool( lv_cn co lv_wch ).
        lv_opn = ct_ops[ lv_te + 1 ]-op.
        if lv_opn <> '=' or lv_iwn = abap_true.
          lv_te += 1.
        else.
          exit.
        endif.
      endwhile.
      clear: lv_dc, lv_ic, lv_ot, lv_nt.
      lv_tk = lv_ts.
      while lv_tk <= lv_te.
        lv_opk = ct_ops[ lv_tk ]-op.
        lv_ec  = ct_ops[ lv_tk ]-text.
        case lv_opk.
          when '-'.
            lv_ot = lv_ot && lv_ec.
            lv_dc += 1.
          when '+'.
            lv_nt = lv_nt && lv_ec.
            lv_ic += 1.
          when '='.
            lv_ot = lv_ot && lv_ec.
            lv_nt = lv_nt && lv_ec.
        endcase.
        lv_tk += 1.
      endwhile.
      if lv_dc > 0 and lv_ic > 0 and lv_dc + lv_ic > 2.
        if lv_ot is not initial.
          append value ty_diff_op( op = '-' text = lv_ot ) to lt_result.
        endif.
        if lv_nt is not initial.
          append value ty_diff_op( op = '+' text = lv_nt ) to lt_result.
        endif.
      else.
        lv_tk = lv_ts.
        while lv_tk <= lv_te.
          append ct_ops[ lv_tk ] to lt_result.
          lv_tk += 1.
        endwhile.
      endif.
      lv_ts = lv_te + 1.
    endwhile.
    ct_ops = lt_result.
  endmethod.
endclass.

class zcl_ave_popup_data implementation.
  method get_user_name.
    result = new zcl_ave_author( )->get_name( iv_user ).
  endmethod.
  method get_latest_author.
    data(lo_vrsd) = new zcl_ave_vrsd( type = i_type name = i_name ).
    if lo_vrsd->vrsd_list is initial. return. endif.
    data(lt_list) = lo_vrsd->vrsd_list.
    sort lt_list by versno descending.
    result = lt_list[ 1 ]-author.
  endmethod.
  method check_part_exists.
    if i_type = 'RELE'.
      result = abap_true.
      return.
    endif.

    " METH: check existence directly in SEOCOMPO (class/method component table)
    if i_type = 'METH' and i_class_name is not initial.
      data lv_meth_cmpname type seocmpname.
      data lv_cmptype      type seocmptype value '1'.
      lv_meth_cmpname = i_name.
      select single clsname from seocompo
        where clsname = @i_class_name
          and cmpname = @lv_meth_cmpname
          and cmptype = @lv_cmptype
        into @data(lv_cls_found).
      result = boolc( sy-subrc = 0 ).
      return.
    endif.

    if i_type = 'CPUB' or i_type = 'CPRO' or i_type = 'CPRI'.
      result = abap_true.
      return.
    endif.

    data lv_tadir_type type tadir-object.
    if i_type = 'REPS'.
      lv_tadir_type = 'PROG'.
    elseif i_type = 'CLSD'.
      lv_tadir_type = 'CLAS'.   " VRSD 'CLSD' = class header, exists as CLAS in TADIR/TR
    else.
      lv_tadir_type = i_type.
    endif.

    data lv_obj_name type tadir-obj_name.
    lv_obj_name = i_name.
    data lv_pgmid type tadir-pgmid.
    select single pgmid from tadir
      where pgmid    = 'R3TR'
        and object   = @lv_tadir_type
        and obj_name = @lv_obj_name
        and delflag  = ' '
      into @lv_pgmid.
    result = boolc( sy-subrc = 0 ).
  endmethod.
  method get_type_text.
    if mv_cache_loaded = abap_false.
      load_type_cache( ).
    endif.
    read table mt_type_cache assigning field-symbol(<c>) with table key type = i_type.
    if sy-subrc = 0.
      result = <c>-text.
    endif.
  endmethod.
  method load_type_cache.
    mv_cache_loaded = abap_true.
    data lt_types_out type standard table of ko100.
    call function 'TRINT_OBJECT_TABLE'
      exporting
        iv_complete  = 'X'
      tables
        tt_types_out = lt_types_out.
    loop at lt_types_out into data(ls_ko100).
      insert value #( type = ls_ko100-object text = ls_ko100-text )
        into table mt_type_cache.
    endloop.
  endmethod.
  method remove_duplicate_versions.
    types: begin of ty_prev,
             objtype    type versobjtyp,
             objname    type versobjnam,
             norm_src   type string_table,
             raw_src    type abaptxt255_tab,
             has_src    type abap_bool,
             base_idx   type i,
             owner      type versuser,
             owner_name type ad_namtext,
             datum      type versdate,
             zeit       type verstime,
             work_idx   type i,
           end of ty_prev.
    types: begin of ty_work,
             row      type zif_ave_popup_types=>ty_version_row,
             norm_src type string_table,
             raw_src  type abaptxt255_tab,
             orig_idx type i,
             check    type abap_bool,
             keep     type abap_bool,
             base     type abap_bool,
           end of ty_work.
    data lt_prev_map type hashed table of ty_prev with unique key objtype objname.
    data lt_result   type zif_ave_popup_types=>ty_t_version_row.
    data lt_work     type standard table of ty_work with default key.
    field-symbols <ver> type ty_work.
    field-symbols <p>   type ty_prev.

    " ct_versions can contain rows for multiple (objtype,objname) pairs mixed
    " together (e.g. all methods of a class sorted globally by versno).
    " Analyze chronologically so duplicate runs keep the earliest version.
    loop at ct_versions into data(ls_input_ver).
      data ls_work type ty_work.
      ls_work-row = ls_input_ver.
      ls_work-orig_idx = sy-tabix.
      append ls_work to lt_work.
    endloop.
    sort lt_work by row-objtype row-objname row-versno ascending row-datum ascending row-zeit ascending.

    if i_keep_korrnum is initial.
      loop at lt_work assigning <ver>.
        <ver>-check = abap_true.
      endloop.
    else.
      data(lv_group_start) = 1.
      while lv_group_start <= lines( lt_work ).
        read table lt_work into data(ls_group) index lv_group_start.
        data(lv_group_end) = lv_group_start.
        data(lv_selected_idx) = 0.

        while lv_group_end <= lines( lt_work ).
          read table lt_work assigning field-symbol(<group_ver>) index lv_group_end.
          if <group_ver>-row-objtype <> ls_group-row-objtype
          or <group_ver>-row-objname <> ls_group-row-objname.
            exit.
          endif.
          if <group_ver>-row-korrnum = i_keep_korrnum.
            lv_selected_idx = lv_group_end.
          endif.
          lv_group_end = lv_group_end + 1.
        endwhile.

        if lv_selected_idx > 0.
          data(lv_prev_k_idx) = 0.
          data(lv_scan_idx) = lv_selected_idx - 1.
          while lv_scan_idx >= lv_group_start.
            read table lt_work assigning <group_ver> index lv_scan_idx.
            if <group_ver>-row-trfunction = 'K'.
              lv_prev_k_idx = lv_scan_idx.
              exit.
            endif.
            lv_scan_idx = lv_scan_idx - 1.
          endwhile.

          data(lv_check_from) = cond i(
            when lv_prev_k_idx > lv_group_start then lv_prev_k_idx - 1
            else lv_group_start ).
          data(lv_mark_idx) = lv_check_from.
          while lv_mark_idx <= lv_selected_idx.
            read table lt_work assigning <group_ver> index lv_mark_idx.
            <group_ver>-check = abap_true.
            if lv_mark_idx = lv_check_from.
              <group_ver>-base = abap_true.
            endif.
            lv_mark_idx = lv_mark_idx + 1.
          endwhile.
        endif.

        lv_group_start = lv_group_end.
      endwhile.
    endif.

    data(lv_total) = 0.
    loop at lt_work transporting no fields where check = abap_true.
      lv_total = lv_total + 1.
    endloop.
    data(lv_check_idx) = 0.

    loop at lt_work assigning <ver>.
      data(lv_work_idx) = sy-tabix.
      if <ver>-check <> abap_true.
        <ver>-keep = abap_true.
        continue.
      endif.

      lv_check_idx = lv_check_idx + 1.
      if lv_check_idx = 1 or lv_check_idx = lv_total or lv_check_idx mod 5 = 0.
        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = conv i( lv_check_idx * 100 / cond i( when lv_total > 0 then lv_total else 1 ) )
            text       = conv char70( |Checking duplicates { <ver>-row-objtype } { <ver>-row-objname } ({ lv_check_idx }/{ lv_total })| ).
      endif.

      " Read source directly from SVRS — bypass zcl_ave_version constructor,
      " whose load_latest_task can raise zcx_ave and leave lt_cur_src empty
      " for some versions while others succeed, producing spurious diffs.
      data lt_cur_src type abaptxt255_tab.
      clear lt_cur_src.
      if <ver>-row-objtype = 'DDLS'.
        lt_cur_src = zcl_ave_version=>load_ddls_source(
          i_objname = <ver>-row-objname
          i_versno  = <ver>-row-versno ).
      else.
        data lt_trdir type trdir_it.
        data(lv_db_no) = zcl_ave_versno=>to_internal( <ver>-row-versno ).
        call function 'SVRS_GET_REPS_FROM_OBJECT'
          exporting
            object_name = <ver>-row-objname
            object_type = <ver>-row-objtype
            versno      = lv_db_no
          tables
            repos_tab   = lt_cur_src
            trdir_tab   = lt_trdir
          exceptions
            no_version  = 1
            others      = 2.
        if sy-subrc <> 0. clear lt_cur_src. endif.
      endif.

      " Fast path for case-sensitive mode: compare raw source tables directly.
      " The normalized path ignores leading whitespace from pretty-printer reindent.
      data lt_cur_norm  type string_table.
      data lt_prev_norm type string_table.
      data lt_prev_raw  type abaptxt255_tab.
      clear lt_cur_norm. clear lt_prev_norm.
      clear lt_prev_raw.
      if i_ignore_case = abap_true.
        loop at lt_cur_src into data(ls_cn).
          data(lv_cn) = conv string( ls_cn ).
          shift lv_cn left deleting leading ` `.
          append lv_cn to lt_cur_norm.
        endloop.
        <ver>-norm_src = lt_cur_norm.
      else.
        <ver>-raw_src = lt_cur_src.
      endif.

      data lv_has_prev type abap_bool.
      lv_has_prev = abap_false.
      unassign <p>.
      read table lt_prev_map assigning <p>
        with table key objtype = <ver>-row-objtype objname = <ver>-row-objname.
      if sy-subrc = 0 and <p>-has_src = abap_true.
        lv_has_prev = abap_true.
        if i_ignore_case = abap_true.
          lt_prev_norm = <p>-norm_src.
        else.
          lt_prev_raw = <p>-raw_src.
        endif.
      endif.

      data(lv_is_duplicate) = cond abap_bool(
        when lv_has_prev = abap_true
         and ( ( i_ignore_case = abap_true and lt_cur_norm = lt_prev_norm )
            or ( i_ignore_case = abap_false and lt_cur_src = lt_prev_raw ) )
        then abap_true
        else abap_false ).
      data(lv_keep_korrnum) = cond abap_bool(
        when i_keep_korrnum is not initial and <ver>-row-korrnum = i_keep_korrnum then abap_true
        else abap_false ).
      data(lv_k_over_t) = cond abap_bool(
        when lv_is_duplicate = abap_true
         and <p> is assigned
         and <p>-work_idx is not initial
         and <p>-base_idx is initial
         and <ver>-row-trfunction = 'K'
         and lt_work[ <p>-work_idx ]-row-trfunction = 'T'
         and lt_work[ <p>-work_idx ]-base <> abap_true
         and ( i_keep_korrnum is initial or lt_work[ <p>-work_idx ]-row-korrnum <> i_keep_korrnum )
        then abap_true
        else abap_false ).

      if lv_is_duplicate = abap_true and <p> is assigned.
        <ver>-row-obj_owner      = <p>-owner.
        <ver>-row-obj_owner_name = <p>-owner_name.
*        <ver>-row-datum          = <p>-datum.
*        <ver>-row-zeit           = <p>-zeit.
      endif.

      if lv_has_prev = abap_false or lv_is_duplicate = abap_false or lv_keep_korrnum = abap_true or lv_k_over_t = abap_true.
        <ver>-keep = abap_true.
        if lv_k_over_t = abap_true.
          lt_work[ <p>-work_idx ]-keep = abap_false.
          <p>-norm_src   = lt_cur_norm.
          <p>-raw_src    = lt_cur_src.
          <p>-has_src    = abap_true.
          <p>-base_idx   = cond #( when <ver>-base = abap_true then lv_work_idx else 0 ).
          <p>-owner      = <ver>-row-obj_owner.
          <p>-owner_name = <ver>-row-obj_owner_name.
          <p>-datum      = <ver>-row-datum.
          <p>-zeit       = <ver>-row-zeit.
          <p>-work_idx   = lv_work_idx.
        elseif lv_is_duplicate = abap_false.
          if <p> is assigned.
            <p>-norm_src   = lt_cur_norm.
            <p>-raw_src    = lt_cur_src.
            <p>-has_src    = abap_true.
            <p>-base_idx   = cond #( when <ver>-base = abap_true then lv_work_idx else 0 ).
            <p>-owner      = <ver>-row-obj_owner.
            <p>-owner_name = <ver>-row-obj_owner_name.
            <p>-datum      = <ver>-row-datum.
            <p>-zeit       = <ver>-row-zeit.
            <p>-work_idx   = lv_work_idx.
          else.
            insert value #( objtype    = <ver>-row-objtype
                            objname    = <ver>-row-objname
                            norm_src   = lt_cur_norm
                            raw_src    = lt_cur_src
                            has_src    = abap_true
                            base_idx   = cond #( when <ver>-base = abap_true then lv_work_idx else 0 )
                            owner      = <ver>-row-obj_owner
                            owner_name = <ver>-row-obj_owner_name
                            datum      = <ver>-row-datum
                            zeit       = <ver>-row-zeit
                            work_idx   = lv_work_idx )
              into table lt_prev_map.
          endif.
        endif.
      endif.
      unassign <p>.
    endloop.

    sort lt_work by orig_idx ascending.
    loop at lt_work assigning <ver> where keep = abap_true.
      append <ver>-row to lt_result.
    endloop.

    ct_versions = lt_result.
  endmethod.
  method get_active_line_count.
    data lv_incname type progname.
    data lt_src type table of string.
    try.
        case i_type.
          when 'CLSD' or 'RELE' or 'DEVC' or 'FUGR' or 'CLAS'.
            " Aggregate / header types — no single source.
            return.
          when 'DDLS'.
            result = lines( zcl_ave_version=>load_ddls_source(
              i_objname = i_name
              i_versno  = zcl_ave_version=>c_version-active ) ).
            return.
          when 'INTF'.
            lv_incname = cl_oo_classname_service=>get_interfacepool_name( conv #( i_name ) ).
          when 'CPUB'.
            lv_incname = cl_oo_classname_service=>get_pubsec_name( conv #( i_name ) ).
          when 'CPRO'.
            lv_incname = cl_oo_classname_service=>get_prosec_name( conv #( i_name ) ).
          when 'CPRI'.
            lv_incname = cl_oo_classname_service=>get_prisec_name( conv #( i_name ) ).
          when 'METH'.
            " i_name layout (VRSD convention): class (30-char, blank-padded) + method
            data(lv_cls) = conv seoclsname( i_name(30) ).
            data lv_mtd type seocpdname.
            lv_mtd = i_name+30.
            lv_incname = cl_oo_classname_service=>get_method_include(
              mtdkey = value #( clsname = lv_cls cpdname = lv_mtd ) ).
          when others.
            lv_incname = i_name.
        endcase.
        if lv_incname is initial. return. endif.
        read report lv_incname into lt_src.
        if sy-subrc = 0.
          result = lines( lt_src ).
        endif.
      catch cx_root.
    endtry.
  endmethod.
  method get_ver_source.
    data lt_vrsd type vrsd_tab.
    data(lv_vno) = zcl_ave_versno=>to_internal( i_versno ).
    select * from vrsd
      where objtype = @i_objtype
        and objname = @i_objname
        and versno  = @lv_vno
      into table @lt_vrsd up to 1 rows.
    if lt_vrsd is initial.
      " Synthetic VRSD row so SVRS_GET_REPS_FROM_OBJECT can still resolve the source.
      append value vrsd(
        objtype = i_objtype
        objname = i_objname
        versno  = lv_vno
        korrnum = i_korrnum
        author  = i_author
        datum   = i_datum
        zeit    = i_zeit
      ) to lt_vrsd.
    endif.
    result = new zcl_ave_version( lt_vrsd[ 1 ] )->get_source( ).
  endmethod.
  method check_class_has_author.
    try.
        data(lo_obj) = new zcl_ave_object_factory( )->get_instance(
          object_type = zcl_ave_object_factory=>gc_type-class
          object_name = conv #( i_class_name ) ).
        loop at lo_obj->get_parts( ) into data(ls_part).
          check ls_part-type <> 'CLSD' and ls_part-type <> 'RELE'.
          if is_substantive_user_change(
               it_versions = build_versions_for_check( i_type = ls_part-type i_name = ls_part-object_name )
               i_type      = ls_part-type
               i_name      = ls_part-object_name
               i_korrnum   = i_korrnum ) = abap_true.
            result = abap_true.
            return.
          endif.
        endloop.
      catch cx_root.
    endtry.
  endmethod.
  method build_versions_for_check.
    try.
        data(lo_vrsd) = new zcl_ave_vrsd( type = i_type name = i_name no_toc = mv_no_toc ignore_unreleased = abap_true ).
      catch zcx_ave.
        return.
    endtry.

    " vrsd_list already has versno (external), korrnum, objtype, objname — no zcl_ave_version needed.
    loop at lo_vrsd->vrsd_list into data(ls_vrsd).
      append value zif_ave_popup_types=>ty_version_row(
        versno  = ls_vrsd-versno
        korrnum = ls_vrsd-korrnum
        objtype = ls_vrsd-objtype
        objname = ls_vrsd-objname ) to result.
    endloop.

    sort result by versno descending.

    " Fill trfunction from E070 — one SELECT per unique korrnum
    loop at result assigning field-symbol(<v>).
      check <v>-korrnum is not initial and <v>-trfunction is initial.
      select single trfunction from e070
        where trkorr = @<v>-korrnum
        into @<v>-trfunction.
      " Propagate trfunction to all versions with same korrnum
      loop at result assigning field-symbol(<v2>) where korrnum = <v>-korrnum and trfunction is initial.
        <v2>-trfunction = <v>-trfunction.
      endloop.
    endloop.
  endmethod.
  method is_substantive_user_change.
    " it_versions is already sorted newest-first with trfunction filled.
    " Find the target version (latest or i_korrnum) and nearest prior K-type version.
    if it_versions is initial. return. endif.

    data ls_latest like line of it_versions.
    if i_korrnum is initial.
      ls_latest = it_versions[ 1 ].
    else.
      loop at it_versions into ls_latest where korrnum = i_korrnum.
        exit.
      endloop.
      if ls_latest is initial.
        return.
      endif.
    endif.

    data ls_prior like ls_latest.
    loop at it_versions into ls_prior
      where versno < ls_latest-versno and trfunction = 'K'.
      exit.
    endloop.
    if ls_prior is initial.
      result = abap_true.
      return.
    endif.

    data lt_new type abaptxt255_tab.
    data lt_old type abaptxt255_tab.
    if i_type = 'DDLS'.
      lt_new = zcl_ave_version=>load_ddls_source( i_objname = i_name i_versno = ls_latest-versno ).
      lt_old = zcl_ave_version=>load_ddls_source( i_objname = i_name i_versno = ls_prior-versno ).
    else.
      data lt_trdir type trdir_it.
      call function 'SVRS_GET_REPS_FROM_OBJECT'
        exporting
          object_name = i_name
          object_type = i_type
          versno      = zcl_ave_versno=>to_internal( ls_latest-versno )
        tables
          repos_tab   = lt_new
          trdir_tab   = lt_trdir
        exceptions
          no_version  = 1
          others      = 2.
      if sy-subrc <> 0. clear lt_new. endif.
      call function 'SVRS_GET_REPS_FROM_OBJECT'
        exporting
          object_name = i_name
          object_type = i_type
          versno      = zcl_ave_versno=>to_internal( ls_prior-versno )
        tables
          repos_tab   = lt_old
          trdir_tab   = lt_trdir
        exceptions
          no_version  = 1
          others      = 2.
      if sy-subrc <> 0. clear lt_old. endif.
    endif.

    result = boolc( lt_new <> lt_old ).
  endmethod.
endclass.

class zcl_ave_popup implementation.
  method constructor.
    mv_object_type = i_object_type.
    mv_object_name = i_object_name.
    " Member vars already have correct defaults (show_diff/no_toc/compact = X, two_pane = ' ')
    " Override only when settings explicitly provided
    if is_settings is supplied.
      mv_show_diff   = is_settings-show_diff.
      mv_layout      = is_settings-layout.
      mv_two_pane    = is_settings-two_pane.
      mv_no_toc                    = is_settings-no_toc.
      zcl_ave_popup_data=>mv_no_toc = is_settings-no_toc.
      mv_compact     = is_settings-compact.
      mv_remove_dup  = is_settings-remove_dup.
      mv_blame       = is_settings-blame.
      mv_ignore_case = is_settings-ignore_case.
      mv_filter_user = is_settings-filter_user.
      mv_date_from   = is_settings-date_from.
      mv_code_review = is_settings-code_review.
    endif.

  endmethod.
  method show.
    build_layout( ).
    build_parts_list( ).
    build_html_viewer( ).
    build_versions_grid( ).

    " Code Review: auto-open report immediately in maximized view
    if mv_code_review = abap_true and mv_cr_report_html is not initial.
      maximize_html( ).
      set_html( mv_cr_report_html ).
      cl_gui_cfw=>flush( ).
      return.
    endif.

    " Auto-open the first part only for single-object views (class/program/intf/func).
    " For TR / package the user picks a row manually — auto-loading versions for
    " an arbitrary "first" object is slow and usually not what they want.
    if mv_object_type <> zcl_ave_object_factory=>gc_type-tr
       and mv_object_type <> zcl_ave_object_factory=>gc_type-package.
      data(lt_supported) = value string_table(
        ( |REPS| ) ( |METH| ) ( |CLSD| ) ( |CPUB| ) ( |CPRO| )
        ( |CPRI| ) ( |CINC| ) ( |CDEF| ) ( |FUNC| ) ).
      loop at mt_parts into data(ls_first)
        where exists_flag = abap_true.
        check line_exists( lt_supported[ table_line = ls_first-type ] ).
        mv_cur_objtype = ls_first-type.
        mv_cur_objname = ls_first-object_name.
        load_versions( i_objtype = ls_first-type i_objname = ls_first-object_name ).
        refresh_vers( ).
        if mt_versions is not initial.
          ms_base_ver = mt_versions[ 1 ].
          mv_viewed_versno = ms_base_ver-versno.
          if mv_show_diff = abap_true.
            read table mt_versions into data(ls_prev_auto) index 2.
            " No previous version → show as new object (all-green diff vs empty source)
            auto_show_diff_or_source( is_old = ls_prev_auto is_new = ms_base_ver ).
          else.
            show_source( i_objtype = ms_base_ver-objtype
                         i_objname = ms_base_ver-objname
                         i_versno  = ms_base_ver-versno ).
          endif.
          update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
        endif.
        exit.
      endloop.
    endif.

    cl_gui_cfw=>flush( ).
  endmethod.
  method build_layout.

    add 1 to mv_counter.

    create object mo_box
      exporting
        width                       = 1300
        height                      = 345
        top                         = 25
        left                        = 50
        caption                     = |{ mv_object_type }: { mv_object_name }|
        lifetime                    = cl_gui_control=>lifetime_dynpro
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
    if sy-subrc <> 0. return. endif.

    set handler me->on_box_close for mo_box.

    " Outer splitter: row 1 = toolbar, row 2 = content
    data(lo_split_outer) = new cl_gui_splitter_container(
      parent  = mo_box
      rows    = 2
      columns = 1 ).
    lo_split_outer->set_row_height( id = 1 height = 4 ).
    lo_split_outer->set_row_sash( id = 1 type = 0 value = 0 ).
    mo_cont_toolbar = lo_split_outer->get_container( row = 1 column = 1 ).
    data(lo_cont_main) = lo_split_outer->get_container( row = 2 column = 1 ).

    " Wrapper: row 1 = normal layout, row 2 = 2-pane layout (hidden initially)
    mo_split_wrap = new cl_gui_splitter_container(
      parent  = lo_cont_main
      rows    = 2
      columns = 1 ).
    mo_split_wrap->set_row_height( id = 1 height = 100 ).
    mo_split_wrap->set_row_height( id = 2 height = 0 ).
    mo_split_wrap->set_row_sash( id = 1 type = 0 value = 0 ).
    mo_split_wrap->set_row_sash( id = 2 type = 0 value = 0 ).
    data(lo_normal) = mo_split_wrap->get_container( row = 1 column = 1 ).
    data(lo_2pane)  = mo_split_wrap->get_container( row = 2 column = 1 ).

    " ── Normal layout: [parts+vers | html] ──────────────────────────
    create object mo_split_main
      exporting
        parent  = lo_normal
        rows    = 1
        columns = 2.
    mo_split_main->set_column_width( id = 1 width = 40 ).
    mo_split_main->set_column_width( id = 2 width = 60 ).
    data(lo_top) = mo_split_main->get_container( row = 1 column = 1 ).
    create object mo_split_top
      exporting
        parent  = lo_top
        rows    = 2
        columns = 1.
    mo_split_top->set_row_height( id = 1 height = 60 ).
    mo_cont_parts = mo_split_top->get_container( row = 1 column = 1 ).
    mo_cont_vers  = mo_split_top->get_container( row = 2 column = 1 ).
    mo_cont_html  = mo_split_main->get_container( row = 1 column = 2 ).

    " ── 2-pane layout: [parts | vers] top + [html] bottom ───────────
    mo_split_2p_wrap = new cl_gui_splitter_container(
      parent  = lo_2pane
      rows    = 2
      columns = 1 ).
    data(lo_2p_wrap) = mo_split_2p_wrap.
    lo_2p_wrap->set_row_height( id = 1 height = 35 ).
    mo_split_2p_top = new cl_gui_splitter_container(
      parent  = lo_2p_wrap->get_container( row = 1 column = 1 )
      rows    = 1
      columns = 2 ).
    mo_split_2p_top->set_column_width( id = 1 width = 25 ).
    mo_split_2p_top->set_column_width( id = 2 width = 75 ).
    mo_cont_parts_2p = mo_split_2p_top->get_container( row = 1 column = 1 ).
    mo_cont_vers_2p  = mo_split_2p_top->get_container( row = 1 column = 2 ).
    mo_cont_html_2p  = lo_2p_wrap->get_container( row = 2 column = 1 ).

    " If starting in TOP-DOWN layout — flip wrapper and point containers
    if mv_layout = abap_false.
      mo_split_wrap->set_row_height( id = 1 height = 0 ).
      mo_split_wrap->set_row_height( id = 2 height = 100 ).
      mo_cont_parts = mo_cont_parts_2p.
      mo_cont_vers  = mo_cont_vers_2p.
      mo_cont_html  = mo_cont_html_2p.
    endif.

    " For single-object types (program / function) — hide parts, give versions 100%
    if mv_object_type = zcl_ave_object_factory=>gc_type-program or
       mv_object_type = zcl_ave_object_factory=>gc_type-function.
      mo_split_top->set_row_height(    id = 1 height = 0   ).
      mo_split_top->set_row_height(    id = 2 height = 100 ).
      mo_split_2p_top->set_column_width( id = 1 width  = 0   ).
      mo_split_2p_top->set_column_width( id = 2 width  = 100 ).
    endif.
  endmethod.
  method build_parts_list.
    " Load parts via object handler factory
    try.
        if mv_object_type = zcl_ave_object_factory=>gc_type-class.
          " CLASS: filter empty includes, no existence check needed
          mt_parts = get_class_parts( conv #( mv_object_name ) ).
        else.
          data(lo_obj) = new zcl_ave_object_factory( )->get_instance(
            object_type = mv_object_type
            object_name = conv #( mv_object_name ) ).
          data(lv_is_tr) = boolc( mv_object_type = zcl_ave_object_factory=>gc_type-tr ).
          loop at lo_obj->get_parts( ) into data(ls_raw).
            check ls_raw-type <> 'RELE'.
            data(lv_exists) = cond abap_bool(
              when lv_is_tr = abap_true
              then zcl_ave_popup_data=>check_part_exists(
                     i_type       = ls_raw-type
                     i_name       = conv #( ls_raw-unit )
                     i_class_name = conv #( ls_raw-class ) )
              else abap_true ).
            data ls_row type ty_part_row.
            ls_row-class       = ls_raw-class.
            ls_row-name        = ls_raw-unit.
            ls_row-type        = ls_raw-type.
            ls_row-type_text   = zcl_ave_popup_data=>get_type_text( ls_raw-type ).
            ls_row-object_name = ls_raw-object_name.
            ls_row-exists_flag = lv_exists.
            ls_row-rows        = cond i( when lv_exists = abap_true
              then zcl_ave_popup_data=>get_active_line_count( i_type = ls_raw-type i_name = ls_raw-object_name )
              else 0 ).
            if lv_exists = abap_false.
              ls_row-rowcolor = 'C601'.   " red
            else.
              data(lv_changed) = cond abap_bool(
                when ls_raw-type = 'CLAS'
                then zcl_ave_popup_data=>check_class_has_author(
                       i_class_name = conv #( ls_raw-object_name )
                       i_korrnum    = cond #( when lv_is_tr = abap_true then conv verskorrno( mv_object_name ) ) )
                else zcl_ave_popup_data=>is_substantive_user_change(
                       it_versions = zcl_ave_popup_data=>build_versions_for_check( i_type = ls_raw-type i_name = ls_raw-object_name )
                       i_type      = ls_raw-type
                       i_name      = ls_raw-object_name
                       i_korrnum   = cond #( when lv_is_tr = abap_true then conv verskorrno( mv_object_name ) ) ) ).
              if lv_changed = abap_true.
                ls_row-rowcolor = 'C510'. " green
              endif.
            endif.
            if ls_raw-type <> 'METH' and ls_raw-type <> 'CPUB'  and ls_raw-type <> 'CPRO' and ls_raw-type <> 'CPRI' and
               ls_raw-type <> 'REPS' and ls_raw-type <> 'PROG' and ls_raw-type <> 'CLSD' and ls_raw-type <> 'CLAS' and
               ls_raw-type <> 'DDLS'.

              ls_row-rowcolor = 'C201'. " not supported obj
            endif.
            append ls_row to mt_parts.
            clear ls_row.
          endloop.
        endif.
      catch zcx_ave.
        " leave mt_parts empty – no crash
    endtry.

    if mv_code_review = abap_true.
      delete mt_parts where rowcolor <> 'C510'.
      clear: mt_acr_stats, mt_hunk_info, mt_hunk_threads,
             mt_approved, mt_declined, mt_decline_notes,
             mv_cr_base_html, mv_cr_cur_key, mv_decline_view_user.
      mv_cr_prepared = abap_false.
      mv_cr_report_html = build_cr_object_report_html( ).

      " Insert REPORT pseudo-part at the top of the list
      data(lv_total_acr) = lines( mt_parts ).
      data(ls_rpt) = value ty_part_row(
        type      = 'RPT'
        name      = |[ Code Review Report - { lv_total_acr } object(s) ]|
        type_text = 'Report'
        rows      = lv_total_acr ).
      insert ls_rpt into mt_parts index 1.
    endif.

    " ── Toolbar (full-width top row, container from build_layout) ──
    create object mo_toolbar exporting parent = mo_cont_toolbar.
    data lt_tb_events type cntl_simple_events.
    append value #( eventid = cl_gui_toolbar=>m_id_function_selected ) to lt_tb_events.
    mo_toolbar->set_registered_events( lt_tb_events ).
    set handler me->on_toolbar_click for mo_toolbar.
    if mv_code_review = abap_true.
      mo_toolbar->add_button_group( value ttb_button(
        ( function  = 'PANE_TOGGLE'
          icon      = conv #( icon_spool_request )
          text      = 'Inline'
          quickinfo = 'Inline' )
        ( function  = 'COMPACT_TOGGLE'
          icon      = conv #( icon_collapse_all )
          text      = 'Compact'
          quickinfo = 'Compact' )
        ( function  = 'FOCUS_TOGGLE'
          icon      = conv #( icon_view_maximize )
          text      = 'Maximize View'
          quickinfo = 'Hide parts/versions, expand HTML' )
        ( function  = 'INFO'
          icon      = conv #( icon_bw_gis )
          text      = ''
          quickinfo = 'Documentation' ) ) ).
      mo_toolbar->add_button_group( value ttb_button(
        ( function  = 'SAVE_REVIEW'
          icon      = conv #( icon_system_save )
          text      = 'Save'
          quickinfo = 'Save review' ) ) ).
    else.
      mo_toolbar->add_button_group( value ttb_button(
        ( function  = 'REFRESH'
          icon      = conv #( icon_refresh )
          text      = 'Refresh'
          quickinfo = 'Refresh' )
        ( function  = 'PANE_TOGGLE'
          icon      = conv #( icon_spool_request )
          text      = 'Inline'
          quickinfo = 'Inline' )
        ( function  = 'DIFF_TOGGLE'
          icon      = conv #( icon_compare )
          text      = 'Show Diff'
          quickinfo = 'Show Diff' )
        ( function  = 'COMPACT_TOGGLE'
          icon      = conv #( icon_collapse_all )
          text      = 'Compact'
          quickinfo = 'Compact' )
        ( function  = 'BLAME_TOGGLE'
          icon      = conv #( icon_history )
          text      = 'Blame'
          quickinfo = 'Toggle Blame' )
        ( function  = 'FOCUS_TOGGLE'
          icon      = conv #( icon_view_maximize )
          text      = 'Maximize View'
          quickinfo = 'Hide parts/versions, expand HTML' )
        ( function  = 'DEBUG'
          icon      = conv #( icon_bw_dm_aa )
          text      = 'Debug'
          quickinfo = 'Show diff ops + pairing decisions' )
        ( function  = 'INFO'
          icon      = conv #( icon_bw_gis )
          text      = ''
          quickinfo = 'Documentation' ) ) ).
    endif.

    " Sync button texts with initial flag values
    mo_toolbar->set_button_info( exporting fcode = 'COMPACT_TOGGLE'
                                           text  = cond #( when mv_compact = abap_true then 'Compact' else 'Full' ) ).
    mo_toolbar->set_button_info( exporting fcode = 'PANE_TOGGLE'
                                           text  = cond #( when mv_two_pane = abap_true then '2-Pane' else 'Inline' ) ).
    if mv_code_review = abap_false.
      mo_toolbar->set_button_info( exporting fcode = 'DIFF_TOGGLE'
                                             text  = cond #( when mv_show_diff = abap_true then 'Show Diff' else 'Show Vers' ) ).
      mo_toolbar->set_button_info( exporting fcode = 'BLAME_TOGGLE'
                                             text  = cond #( when mv_blame = abap_true then 'Blame ON' else 'Blame' ) ).
    endif.

    create_parts_alv( ).
  endmethod.
  method create_parts_alv.
    " ── Field catalog ──
    data lt_fcat type lvc_t_fcat.
    data ls_fc   type lvc_s_fcat.

    clear ls_fc. ls_fc-fieldname = 'TYPE'.        ls_fc-coltext = 'Type'.
    ls_fc-outputlen = 6.  append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'NAME'.        ls_fc-coltext = 'Object'.
    ls_fc-outputlen = 30. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'CLASS'.       ls_fc-coltext = 'Class'.
    ls_fc-outputlen = 20. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'TYPE_TEXT'.   ls_fc-coltext = 'Type Description'.
    ls_fc-outputlen = 30. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'ROWS'.        ls_fc-coltext = 'Rows'.
    ls_fc-outputlen = 6. ls_fc-just = 'R'. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'OBJECT_NAME'. ls_fc-coltext = 'Object'.
    ls_fc-no_out = abap_true. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'EXISTS_FLAG'. ls_fc-coltext = 'Exists'.
    ls_fc-no_out = abap_true. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'ROWCOLOR'.    ls_fc-coltext = 'Color'.
    ls_fc-no_out = abap_true. append ls_fc to lt_fcat.

    " ── Layout ──
    data ls_layo type lvc_s_layo.
    ls_layo-zebra      = abap_true.
    ls_layo-info_fname = 'ROWCOLOR'.
    ls_layo-cwidth_opt = abap_true.
    ls_layo-no_toolbar = abap_false.
    ls_layo-sel_mode   = 'A'.

    " ── Create ALV Grid ──
    mo_alv_parts = new cl_gui_alv_grid( i_parent = mo_cont_parts ).

    set handler me->handle_parts_toolbar  for mo_alv_parts.
    set handler me->handle_parts_command  for mo_alv_parts.
    set handler me->handle_parts_dblclick for mo_alv_parts.

    mo_alv_parts->set_table_for_first_display(
      exporting
        is_layout       = ls_layo
        i_save          = 'A'
        i_default       = 'X'
      changing
        it_fieldcatalog = lt_fcat
        it_outtab       = mt_parts ).

    mo_alv_parts->set_toolbar_interactive( ).
  endmethod.
  method build_html_viewer.
    create_html_viewer( ).
  endmethod.
  method create_html_viewer.
    " Split mo_cont_html into two rows: HTML on top (diff), ABAP editor
    " on bottom (single-version source). Only one has non-zero height.
    create object mo_split_html
      exporting
        parent  = mo_cont_html
        rows    = 2
        columns = 1.
    mo_cont_html_diff = mo_split_html->get_container( row = 1 column = 1 ).
    mo_cont_html_code = mo_split_html->get_container( row = 2 column = 1 ).
    mo_split_html->set_row_height( id = 1 height = 100 ).
    mo_split_html->set_row_height( id = 2 height = 0 ).

    create object mo_html
      exporting
        parent             = mo_cont_html_diff
      exceptions
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        others             = 5.
    data lt_html_ev type cntl_simple_events.
    append value #( eventid = cl_gui_html_viewer=>m_id_sapevent ) to lt_html_ev.
    mo_html->set_registered_events( lt_html_ev ).
    set handler me->on_sapevent for mo_html.

    create object mo_code_viewer
      exporting
        parent           = mo_cont_html_code
        max_number_chars = 255.
    mo_code_viewer->upload_properties( exceptions others = 1 ).
    mo_code_viewer->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
    mo_code_viewer->create_document( ).
    mo_code_viewer->set_readonly_mode( 1 ).

    set_html(
      |<!DOCTYPE html><html><head><style>| &&
      |body\{margin:0;background:#f8f8f8;color:#999;| &&
      |font:13px/1.6 Consolas,monospace;| &&
      |display:flex;align-items:center;justify-content:center;height:100vh\}| &&
      |</style></head><body>| &&
      |<div>Double-click a part on the left to open its latest version.</div>| &&
      |</body></html>| ).
  endmethod.
  method build_versions_grid.
    create_versions_alv( ).
  endmethod.
  method create_versions_alv.
    " ── Field catalog ──
    data lt_fcat type lvc_t_fcat.
    data ls_fc   type lvc_s_fcat.

    clear ls_fc. ls_fc-fieldname = 'VERSNO'.      ls_fc-no_out = abap_true.  append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'VERSNO_TEXT'. ls_fc-coltext = 'Version'.
    ls_fc-outputlen = 8.  append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'DATUM'.       ls_fc-coltext = 'Date'.
    ls_fc-outputlen = 10. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'ZEIT'.        ls_fc-coltext = 'Time'.
    ls_fc-outputlen = 8.  append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'AUTHOR'.      ls_fc-coltext = 'Author'.
    ls_fc-outputlen = 12. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'AUTHOR_NAME'.    ls_fc-coltext = 'Name'.
    ls_fc-outputlen = 20. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'OBJ_OWNER'.      ls_fc-coltext = 'Obj Owner'.
    ls_fc-outputlen = 12. ls_fc-emphasize = 'C401'. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'OBJ_OWNER_NAME'. ls_fc-coltext = 'Owner Name'.
    ls_fc-outputlen = 20. ls_fc-emphasize = 'C401'. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'KORRNUM'.     ls_fc-coltext = 'Request'.
    ls_fc-outputlen = 12. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'TRFUNCTION'.  ls_fc-coltext = 'Type'.
    ls_fc-outputlen = 4.  append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'TASK'.        ls_fc-coltext = 'Task'.
    ls_fc-outputlen = 12. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'KORR_TEXT'.   ls_fc-coltext = 'Description'.
    ls_fc-outputlen = 40. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'OBJNAME'.     ls_fc-coltext = 'Object'.
    ls_fc-outputlen = 30. append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'OBJTYPE'.     ls_fc-coltext = 'Type'.
    ls_fc-outputlen = 6.  append ls_fc to lt_fcat.
    clear ls_fc. ls_fc-fieldname = 'ROWCOLOR'.    ls_fc-no_out = abap_true. append ls_fc to lt_fcat.

    " ── Layout ──
    data ls_layo type lvc_s_layo.
    ls_layo-zebra      = abap_true.
    ls_layo-info_fname = 'ROWCOLOR'.
    ls_layo-cwidth_opt = abap_true.
    ls_layo-sel_mode   = 'A'.

    " ── Create ALV Grid ──
    mo_alv_vers = new cl_gui_alv_grid( i_parent = mo_cont_vers ).

    set handler me->handle_vers_toolbar  for mo_alv_vers.
    set handler me->handle_vers_command  for mo_alv_vers.
    set handler me->handle_vers_dblclick for mo_alv_vers.

    mo_alv_vers->set_table_for_first_display(
      exporting
        is_layout       = ls_layo
        i_save          = 'A'
        i_default       = 'X'
      changing
        it_fieldcatalog = lt_fcat
        it_outtab       = mt_versions ).

    mo_alv_vers->set_toolbar_interactive( ).
  endmethod.
  method handle_parts_toolbar.
    clear e_object->mt_toolbar.
    check mt_parts_backup is not initial.
    append value stb_button(
      function  = 'BACK'
      icon      = conv #( icon_previous_object )
      text      = 'Back'
      quickinfo = 'Back'
      butn_type = 0 ) to e_object->mt_toolbar.
  endmethod.
  method handle_parts_command.
    case e_ucomm.
      when 'BACK'.
        check mt_parts_backup is not initial.
        mt_parts = mt_parts_backup.
        clear: mt_parts_backup, mv_drilled_class.
        refresh_parts( ).
      when others.
        " pass other commands to toolbar handler (REFRESH etc.)
        on_toolbar_click( fcode = e_ucomm ).
    endcase.
  endmethod.
  method handle_parts_dblclick.
    data(lv_row) = es_row_no-row_id.
    read table mt_parts into data(ls_part) index lv_row.
    if sy-subrc <> 0. return. endif.

    " ── Code Reviewer: REPORT pseudo-part ───────────────────────────
    if ls_part-type = 'RPT'.
      maximize_html( ).
      set_html( mv_cr_report_html ).
      return.
    endif.

    " ── Code Reviewer: show pre-cached diff if available ───────────
    if mv_code_review = abap_true.
      read table mt_acr_stats into data(ls_stat)
        with key objtype = ls_part-type obj_name = ls_part-object_name.
      if sy-subrc = 0.
        data(ls_ck) = value ty_diff_cache_key(
          objtype     = ls_stat-objtype
          objname     = ls_stat-obj_name
          versno_o    = ls_stat-versno_old
          versno_n    = ls_stat-versno_new
          blame       = mv_blame
          two_pane    = mv_two_pane
          compact     = mv_compact
          debug       = mv_debug
          ignore_case = mv_ignore_case ).
        read table mt_diff_cache into data(ls_ch) with table key key = ls_ck.
        if sy-subrc = 0.
          mv_cur_objtype   = ls_part-type.
          mv_cur_objname   = ls_part-object_name.
          mv_cur_part_name = cond string(
            when ls_part-class is not initial then |{ ls_part-class } – { ls_part-name }|
            else ls_part-name ).
          mv_cr_cur_key   = |{ ls_stat-objtype }~{ ls_stat-obj_name }|.
          mv_cr_base_html = ls_ch-html.
          " Restore layout (un-maximize) so versions grid is visible
          mv_focus_html = abap_false.
          mo_split_main->set_column_width( id = 1 width = 20 ).
          " Load versions for this part so the grid is populated
          load_versions( i_objtype = ls_part-type i_objname = ls_part-object_name ).
          refresh_vers( ).
          set_html( inject_approve_btn( iv_html = ls_ch-html iv_key = mv_cr_cur_key ) ).
          return.
        endif.
      endif.
      " No cache — fall through to standard Version Explorer diff mechanism
    endif.

    " ── CLAS row (from TR) ──────────────────────────────────────────
    if ls_part-type = 'CLAS'.
      if ls_part-exists_flag = abap_false.
        set_html(
          |<!DOCTYPE html><html><head><style>| &&
          |body\{font:13px/1.8 Consolas,sans-serif;background:#fff8f8;| &&
          |padding:24px;color:#333\}| &&
          |h3\{color:#c0392b;margin-bottom:8px\}| &&
          |.lbl\{color:#888;font-size:11px\}.val\{font-weight:bold\}| &&
          |</style></head><body>| &&
          |<h3>&#9888; Object not found in system</h3>| &&
          |<p><span class="lbl">Type:</span> <span class="val">CLAS</span></p>| &&
          |<p><span class="lbl">Name:</span> | &&
          |<span class="val">{ ls_part-object_name }</span></p>| &&
          |</body></html>| ).
      else.
        mt_parts_backup = mt_parts.
        mv_drilled_class = ls_part-object_name.
        clear mt_parts.
        try.
            mt_parts = get_class_parts( i_name = ls_part-object_name ).
          catch zcx_ave.
        endtry.
        refresh_parts( ).
        " Auto-open first part
        read table mt_parts into data(ls_first_part) index 1.
        if sy-subrc = 0.
          mv_cur_objtype   = ls_first_part-type.
          mv_cur_objname   = ls_first_part-object_name.
          mv_cur_part_name = ls_first_part-name.
          load_versions( i_objtype = ls_first_part-type i_objname = ls_first_part-object_name ).
          refresh_vers( ).
          if mt_versions is not initial.
            ms_base_ver = mt_versions[ 1 ].
            mv_viewed_versno = ms_base_ver-versno.
            if mv_show_diff = abap_true.
              read table mt_versions into data(ls_prev_cls) index 2.
              " No previous version → show as new object (all-green diff vs empty source)
              auto_show_diff_or_source( is_old = ls_prev_cls is_new = ms_base_ver ).
            else.
              show_source( i_objtype = ms_base_ver-objtype
                           i_objname = ms_base_ver-objname
                           i_versno  = ms_base_ver-versno ).
            endif.
            update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
          endif.
        endif.
      endif.
      return.
    endif.

    " ── Unsupported object type ───────────────────────────────────
    data(lt_supported) = value string_table(
      ( |REPS| ) ( |METH| ) ( |CLSD| ) ( |CPUB| ) ( |CPRO| )
      ( |CPRI| ) ( |CINC| ) ( |CDEF| ) ( |FUNC| ) ( |DDLS| ) ).
    if not line_exists( lt_supported[ table_line = ls_part-type ] ).
      set_html(
        |<html><body style="font:13px Consolas,sans-serif;| &&
        |padding:24px;color:#666">| &&
        |<h3 style="color:#888">&#128683; Not supported</h3>| &&
        |<p>This object type is not supported at the moment.</p>| &&
        |<p style="color:#aaa">Type: { ls_part-type }</p>| &&
        |</body></html>| ).
      return.
    endif.

    mv_cur_objtype   = ls_part-type.
    mv_cur_objname   = ls_part-object_name.
    mv_cur_part_name = cond string(
      when ls_part-class is not initial and ls_part-class <> mv_object_name
      then |{ ls_part-class } – { ls_part-name }|
      else ls_part-name ).

    " ── Object doesn't exist in system ────────────────────────────
    if ls_part-exists_flag = abap_false.

      " Find last known version date from VRSD
      data lv_last_date type versdate.
      data lv_last_time type verstime.
      data lv_last_auth type versuser.

      select single datum, zeit, author
        from vrsd
        where objtype = @ls_part-type
          and objname = @ls_part-object_name
"ORDER BY datum DESCENDING, zeit DESCENDING

        into (@lv_last_date, @lv_last_time, @lv_last_auth)
        .

      data(lv_last_info) = cond string(
        when sy-subrc = 0
        then |Last version: { lv_last_date } { lv_last_time } by { lv_last_auth }|
        else |No version history found| ).

      set_html(
        |<!DOCTYPE html><html><head><style>| &&
        |body\{font:13px/1.8 Consolas,sans-serif;background:#fff8f8;| &&
        |padding:24px;color:#333\}| &&
        |h3\{color:#c0392b;margin-bottom:8px\}| &&
        |.lbl\{color:#888;font-size:11px\}| &&
        |.val\{font-weight:bold\}| &&
        |</style></head><body>| &&
        |<h3>&#9888; Object not found in system</h3>| &&
        |<p><span class="lbl">Type:</span> | &&
        |<span class="val">{ ls_part-type }</span></p>| &&
        |<p><span class="lbl">Name:</span> | &&
        |<span class="val">{ ls_part-object_name }</span></p>| &&
        |<p><span class="lbl">{ lv_last_info }</span></p>| &&
        |<p style="margin-top:12px;color:#888;font-size:11px">| &&
        |Previous versions are listed below — | &&
        |double-click to view historical source.</p>| &&
        |</body></html>| ).
      return.
    endif.

    " ── Object exists: normal flow ─────────────────────────────────
    load_versions( i_objtype = ls_part-type i_objname = ls_part-object_name ).

    clear ms_base_ver.
    clear mv_viewed_versno.
    if mt_versions is not initial.
      " In TR mode: base = version that belongs to the TR, not necessarily Active.
      if mv_object_type = zcl_ave_object_factory=>gc_type-tr.
        loop at mt_versions into ms_base_ver where korrnum = mv_object_name.
          exit.
        endloop.
      endif.
      if ms_base_ver is initial.
        ms_base_ver = mt_versions[ 1 ].
      endif.
      mv_viewed_versno = ms_base_ver-versno.
    endif.

    update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
    refresh_vers( ).

    if mt_versions is not initial.
      if mv_show_diff = abap_true.
        " Prior = first version before the base (VRSD korrnum is always K-type).
        data ls_prev_part type ty_version_row.
        loop at mt_versions into ls_prev_part where versno < ms_base_ver-versno.
          exit.
        endloop.
        if ls_prev_part is not initial.
          auto_show_diff_or_source( is_old = ls_prev_part is_new = ms_base_ver ).
        else.
          show_source( i_objtype = ms_base_ver-objtype
                       i_objname = ms_base_ver-objname
                       i_versno  = ms_base_ver-versno ).
        endif.
      else.
        show_source( i_objtype = ms_base_ver-objtype
                     i_objname = ms_base_ver-objname
                     i_versno  = ms_base_ver-versno ).
      endif.
    endif.
  endmethod.
  method load_versions.
    if mv_task_view = abap_true.
      load_versions_task_view( i_objtype = i_objtype i_objname = i_objname ).
      return.
    endif.
    clear mt_versions.
    clear mv_cur_creator.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 0
        text       = conv char70( |Loading versions for { i_objtype } { i_objname }| ).

    try.
        data(lo_vrsd) = new zcl_ave_vrsd(
          type      = i_objtype
          name      = i_objname
          no_toc    = abap_false
          date_from = mv_date_from ).
      catch zcx_ave.
        return.
    endtry.

    data(lv_vrsd_total) = lines( lo_vrsd->vrsd_list ).
    loop at lo_vrsd->vrsd_list into data(ls_vrsd).
      if sy-tabix = 1 or sy-tabix = lv_vrsd_total or sy-tabix mod 10 = 0.
        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = conv i( sy-tabix * 20 / cond i( when lv_vrsd_total > 0 then lv_vrsd_total else 1 ) )
            text       = conv char70( |Reading version metadata ({ sy-tabix }/{ lv_vrsd_total })| ).
      endif.
      try.
          data(lo_ver) = new zcl_ave_version( ls_vrsd ).
          append value ty_version_row(
            versno         = lo_ver->version_number
            versno_text    = cond #( when lo_ver->version_number = '99998'
                                     then 'Active'
                                     else conv string( lo_ver->version_number + 0 ) )
            datum          = lo_ver->date
            zeit           = lo_ver->time
            author         = ls_vrsd-author
            author_name    = zcl_ave_popup_data=>get_user_name( ls_vrsd-author )
            obj_owner      = lo_ver->author
            obj_owner_name = lo_ver->author_name
            korrnum        = lo_ver->request
            task           = lo_ver->task
            objtype        = lo_ver->objtype
            objname        = lo_ver->objname ) to mt_versions.
        catch zcx_ave.
          " Skip version if metadata fails
      endtry.
    endloop.

    sort mt_versions by versno descending datum descending zeit descending.

    " Rename versno_text for duplicate special versions (keep newest as-is)
    data lv_seen_active   type abap_bool.
    data lv_seen_modified type abap_bool.
    data lv_active_idx    type i value 1.
    data lv_modified_idx  type i value 1.
    loop at mt_versions assigning field-symbol(<vr>).
      if <vr>-versno = zcl_ave_version=>c_version-active.
        if lv_seen_active = abap_true.
          <vr>-versno_text = |Active ({ lv_active_idx })|.
          lv_active_idx = lv_active_idx + 1.
        else.
          lv_seen_active = abap_true.
        endif.
      elseif <vr>-versno = zcl_ave_version=>c_version-modified.
        if lv_seen_modified = abap_true.
          <vr>-versno_text = |Modified ({ lv_modified_idx })|.
          lv_modified_idx = lv_modified_idx + 1.
        else.
          lv_seen_modified = abap_true.
        endif.
      endif.
    endloop.

    loop at mt_versions assigning field-symbol(<ver_trf>).
      check <ver_trf>-korrnum is not initial and <ver_trf>-trfunction is initial.
      select single trfunction from e070
        where trkorr = @<ver_trf>-korrnum
        into @<ver_trf>-trfunction.
      loop at mt_versions assigning field-symbol(<ver_trf2>)
        where korrnum = <ver_trf>-korrnum and trfunction is initial.
        <ver_trf2>-trfunction = <ver_trf>-trfunction.
      endloop.
    endloop.

    " Strategy:
    "   1. For K requests, read task releases from CORR/RELE rows.
    "   2. For other contexts, fetch type-S tasks that touch this object.
    "   3. For each version: nearest task by date+time from the pre-fetched list.
    " VRSD type (REPS/METH/CLSD/CPUB…) differs from E071 type (PROG/CLAS…),
    " so we map first.
    data lv_trf_s type e070-trfunction value 'S'.

    " Build E071 key set for this object (map VRSD type -> E071 transport type)
    types: begin of ty_lv_obj_key,
             object   type e071-object,
             obj_name type e071-obj_name,
           end of ty_lv_obj_key.
    types: begin of ty_lv_task_cand,
             trkorr  type trkorr,
             strkorr type trkorr,
             as4user type as4user,
             as4date type as4date,
             as4time type as4time,
           end of ty_lv_task_cand.
    types: begin of ty_rele_object,
             trkorr   type trkorr,
             obj_name type e071-obj_name,
           end of ty_rele_object.
    data lt_lv_keys      type sorted table of ty_lv_obj_key with unique key object obj_name.
    data lt_lv_all_tasks type standard table of ty_lv_task_cand.
    data lt_k_requests type sorted table of trkorr with unique key table_line.
    data lv_oldest_rele_date type as4date.
    data lv_oldest_rele_time type as4time.
    data lv_use_rele_tasks type abap_bool.
    data lv_request_trfunction type e070-trfunction.
    data lv_current_request type trkorr.

    lv_current_request = conv #( mv_object_name ).
    if mv_object_type = zcl_ave_object_factory=>gc_type-tr.
      select single trfunction from e070
        where trkorr = @lv_current_request
        into @lv_request_trfunction.
    endif.

    loop at mt_versions into data(ls_k_request_scan)
      where trfunction = 'K' and korrnum is not initial.
      insert conv trkorr( ls_k_request_scan-korrnum ) into table lt_k_requests.
    endloop.
    if lv_request_trfunction = 'K'.
      insert lv_current_request into table lt_k_requests.
    endif.

    data lv_lv_e071_type type e071-object.
    data lv_lv_e071_name type versobjnam.
    lv_lv_e071_type = switch e071-object( i_objtype
      when 'REPS' or 'REPT'                                then 'PROG'
      when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' then 'CLAS'
      else i_objtype ).
    lv_lv_e071_name = i_objname.
    case i_objtype.
      when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' or 'REPT'.
        data(lv_lv_eq) = find( val = lv_lv_e071_name sub = '=' ).
        if lv_lv_eq > 0.
          lv_lv_e071_name = lv_lv_e071_name(lv_lv_eq).
        endif.
    endcase.

    insert value #( object = lv_lv_e071_type obj_name = lv_lv_e071_name ) into table lt_lv_keys.
    if lv_lv_e071_type = 'PROG'.
      insert value #( object = 'REPS' obj_name = lv_lv_e071_name ) into table lt_lv_keys.
    elseif lv_lv_e071_type = 'REPS'.
      insert value #( object = 'PROG' obj_name = lv_lv_e071_name ) into table lt_lv_keys.
    endif.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 35
        text       = conv char70( |Reading S-requests for { i_objtype } { i_objname }| ).

    if lt_k_requests is not initial.
      data lt_rele_objects type standard table of ty_rele_object.
      data lv_corr_pgmid type e071-pgmid value 'CORR'.
      data lv_corr_rele  type e071-object value 'RELE'.

      select trkorr, obj_name from e071
        for all entries in @lt_k_requests
        where trkorr = @lt_k_requests-table_line
          and pgmid  = @lv_corr_pgmid
          and object = @lv_corr_rele
        into table @lt_rele_objects.

      loop at lt_rele_objects into data(ls_rele_object).
        data lv_rele_task      type string.
        data lv_rele_date_text type string.
        data lv_rele_time_text type string.
        data lv_rele_owner     type string.

        condense ls_rele_object-obj_name.
        split ls_rele_object-obj_name at space
          into lv_rele_task lv_rele_date_text lv_rele_time_text lv_rele_owner.
        check lv_rele_task is not initial
          and strlen( lv_rele_date_text ) = 8
          and strlen( lv_rele_time_text ) = 6
          and lv_rele_date_text co '0123456789'
          and lv_rele_time_text co '0123456789'.

        data ls_rele_task type ty_lv_task_cand.
        ls_rele_task-trkorr   = lv_rele_task.
        ls_rele_task-strkorr  = ls_rele_object-trkorr.
        ls_rele_task-as4user  = lv_rele_owner.
        ls_rele_task-as4date  = lv_rele_date_text.
        ls_rele_task-as4time  = lv_rele_time_text.
        append ls_rele_task to lt_lv_all_tasks.

        if ls_rele_task-strkorr = lv_current_request
           and ( lv_oldest_rele_date is initial
              or ls_rele_task-as4date < lv_oldest_rele_date
              or ( ls_rele_task-as4date = lv_oldest_rele_date
                   and ls_rele_task-as4time < lv_oldest_rele_time ) ).
          lv_oldest_rele_date = ls_rele_task-as4date.
          lv_oldest_rele_time = ls_rele_task-as4time.
        endif.
      endloop.
      lv_use_rele_tasks = boolc( lt_lv_all_tasks is not initial ).
    endif.

    if lt_lv_all_tasks is initial.
      select e070~trkorr, e070~strkorr, e070~as4user, e070~as4date, e070~as4time
        from e071
        inner join e070 on e070~trkorr = e071~trkorr
        for all entries in @lt_lv_keys
        where e071~object     = @lt_lv_keys-object
          and e071~obj_name   = @lt_lv_keys-obj_name
          and e070~trfunction = @lv_trf_s
        into table @lt_lv_all_tasks.
    endif.
    if lv_use_rele_tasks = abap_true.
      sort lt_lv_all_tasks by as4date ascending as4time ascending.
    else.
      sort lt_lv_all_tasks by as4date descending as4time descending.
    endif.

    data(lv_match_total) = lines( mt_versions ).
    loop at mt_versions assigning field-symbol(<ver>).
      if sy-tabix = 1 or sy-tabix = lv_match_total or sy-tabix mod 10 = 0.
        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = 35 + conv i( sy-tabix * 25 / cond i( when lv_match_total > 0 then lv_match_total else 1 ) )
            text       = conv char70( |Matching S-request ({ sy-tabix }/{ lv_match_total })| ).
      endif.

      if lv_use_rele_tasks = abap_true.
        loop at lt_lv_all_tasks into data(ls_cand_rele).
          if <ver>-trfunction = 'K' and ls_cand_rele-strkorr <> <ver>-korrnum.
            continue.
          endif.
          check ls_cand_rele-as4date > <ver>-datum
             or ( ls_cand_rele-as4date = <ver>-datum and ls_cand_rele-as4time >= <ver>-zeit ).
          check ls_cand_rele-as4user is initial or ls_cand_rele-as4user = <ver>-author.
          <ver>-task           = ls_cand_rele-trkorr.
          <ver>-obj_owner      = ls_cand_rele-as4user.
          <ver>-obj_owner_name = zcl_ave_popup_data=>get_user_name( ls_cand_rele-as4user ).
          exit.
        endloop.
        if <ver>-task is initial.
          loop at lt_lv_all_tasks into ls_cand_rele.
            if <ver>-trfunction = 'K' and ls_cand_rele-strkorr <> <ver>-korrnum.
              continue.
            endif.
            check ls_cand_rele-as4user is initial or ls_cand_rele-as4user = <ver>-author.
            <ver>-task           = ls_cand_rele-trkorr.
            <ver>-obj_owner      = ls_cand_rele-as4user.
            <ver>-obj_owner_name = zcl_ave_popup_data=>get_user_name( ls_cand_rele-as4user ).
          endloop.
        endif.
        if <ver>-task is initial.
          loop at lt_lv_all_tasks into ls_cand_rele.
            if <ver>-trfunction = 'K' and ls_cand_rele-strkorr <> <ver>-korrnum.
              continue.
            endif.
            check ls_cand_rele-as4date > <ver>-datum
               or ( ls_cand_rele-as4date = <ver>-datum and ls_cand_rele-as4time >= <ver>-zeit ).
            <ver>-task           = ls_cand_rele-trkorr.
            <ver>-obj_owner      = ls_cand_rele-as4user.
            <ver>-obj_owner_name = zcl_ave_popup_data=>get_user_name( ls_cand_rele-as4user ).
            exit.
          endloop.
        endif.
        if <ver>-task is initial.
          loop at lt_lv_all_tasks into ls_cand_rele.
            if <ver>-trfunction = 'K' and ls_cand_rele-strkorr <> <ver>-korrnum.
              continue.
            endif.
            <ver>-task           = ls_cand_rele-trkorr.
            <ver>-obj_owner      = ls_cand_rele-as4user.
            <ver>-obj_owner_name = zcl_ave_popup_data=>get_user_name( ls_cand_rele-as4user ).
          endloop.
        endif.
      else.
        loop at lt_lv_all_tasks into data(ls_cand).
          check ls_cand-as4date < <ver>-datum
             or ( ls_cand-as4date = <ver>-datum and ls_cand-as4time <= <ver>-zeit ).
          if <ver>-trfunction = 'K' and ls_cand-strkorr <> <ver>-korrnum.
            continue.
          endif.
          <ver>-task           = ls_cand-trkorr.
          <ver>-obj_owner      = ls_cand-as4user.
          <ver>-obj_owner_name = zcl_ave_popup_data=>get_user_name( ls_cand-as4user ).
          exit.
        endloop.
      endif.
    endloop.

    if mv_code_review = abap_true
       and mv_object_type = zcl_ave_object_factory=>gc_type-tr
       and lv_oldest_rele_date is not initial.
      data lv_prev_k_boundary_idx type i.
      loop at mt_versions assigning field-symbol(<ver_cut>).
        check <ver_cut>-datum < lv_oldest_rele_date
           or ( <ver_cut>-datum = lv_oldest_rele_date
                and <ver_cut>-zeit < lv_oldest_rele_time ).
        if <ver_cut>-trfunction = 'K'.
          lv_prev_k_boundary_idx = sy-tabix.
          exit.
        endif.
      endloop.

      if lv_prev_k_boundary_idx > 0.
        data lt_versions_window type ty_t_version_row.
        loop at mt_versions into data(ls_version_window) to lv_prev_k_boundary_idx.
          append ls_version_window to lt_versions_window.
        endloop.
        mt_versions = lt_versions_window.
      endif.
    endif.

    loop at mt_versions assigning field-symbol(<ver_owner_guard>)
      where trfunction = 'K' and task is initial.
      <ver_owner_guard>-obj_owner      = <ver_owner_guard>-author.
      <ver_owner_guard>-obj_owner_name = <ver_owner_guard>-author_name.
    endloop.

    data ls_creator_ver type ty_version_row.
    loop at mt_versions into data(ls_creator_scan).
      if ls_creator_ver is initial or ls_creator_scan-versno < ls_creator_ver-versno.
        ls_creator_ver = ls_creator_scan.
      endif.
    endloop.
    if ls_creator_ver is not initial.
      mv_cur_creator = cond versuser(
        when ls_creator_ver-obj_owner is not initial then ls_creator_ver-obj_owner
        else ls_creator_ver-author ).
    endif.

    " Fill request description and trfunction from E07T / E070
    data lv_korr_text type e07t-as4text.
    loop at mt_versions assigning field-symbol(<ver2>).
      check <ver2>-korrnum is not initial.
      select single as4text from e07t
        where trkorr = @<ver2>-korrnum
          and langu  = @sy-langu
        into @lv_korr_text.
      <ver2>-korr_text = lv_korr_text.

      if <ver2>-trfunction is initial.
        select single trfunction from e070
          where trkorr = @<ver2>-korrnum
          into @<ver2>-trfunction.
      endif.
    endloop.

    if mv_remove_dup = abap_true.
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = 70
          text       = conv char70( |Checking duplicate versions for { i_objtype } { i_objname }| ).
      zcl_ave_popup_data=>remove_duplicate_versions(
        exporting
          i_keep_korrnum = cond #( when mv_object_type = zcl_ave_object_factory=>gc_type-tr
                                   then conv trkorr( mv_object_name ) )
          i_ignore_case  = mv_ignore_case
        changing
          ct_versions    = mt_versions ).
      loop at mt_versions assigning field-symbol(<ver_dup_owner_guard>)
        where trfunction = 'K' and task is initial.
        <ver_dup_owner_guard>-obj_owner      = <ver_dup_owner_guard>-author.
        <ver_dup_owner_guard>-obj_owner_name = <ver_dup_owner_guard>-author_name.
      endloop.
    endif.

    if mv_no_toc = abap_true.
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = 95
          text       = conv char70( |Filtering TOC versions for { i_objtype } { i_objname }| ).
      delete mt_versions where trfunction = 'T'.
    endif.
  endmethod.
  method switch_pane_layout.
    if mv_two_pane = abap_true.
      mo_split_wrap->set_row_height( id = 1 height = 0 ).
      mo_split_wrap->set_row_height( id = 2 height = 100 ).
      mo_cont_parts = mo_cont_parts_2p.
      mo_cont_vers  = mo_cont_vers_2p.
      mo_cont_html  = mo_cont_html_2p.
    else.
      mo_split_wrap->set_row_height( id = 1 height = 100 ).
      mo_split_wrap->set_row_height( id = 2 height = 0 ).
      mo_cont_parts = mo_split_top->get_container( row = 1 column = 1 ).
      mo_cont_vers  = mo_split_top->get_container( row = 2 column = 1 ).
      mo_cont_html  = mo_split_main->get_container( row = 1 column = 2 ).
    endif.
    free mo_alv_parts.
    free mo_alv_vers.
    free mo_code_viewer.
    free mo_html.
    free mo_split_html.
    create_parts_alv( ).
    create_versions_alv( ).
    create_html_viewer( ).
    if mt_versions is not initial.
      update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
      if mv_viewed_versno is not initial.
        read table mt_versions into data(ls_v) with key versno = mv_viewed_versno.
        if sy-subrc = 0.
          if mv_show_diff = abap_true.
            show_versions_diff( is_old = ls_v is_new = ms_base_ver ).
          else.
            show_source( i_objtype = ls_v-objtype i_objname = ls_v-objname i_versno = ls_v-versno ).
          endif.
        endif.
      endif.
    endif.
  endmethod.
  method refresh_parts.
    check mv_refreshing = abap_false.
    mv_refreshing = abap_true.
    data ls_layo_p type lvc_s_layo.
    mo_alv_parts->get_frontend_layout( importing es_layout = ls_layo_p ).
    ls_layo_p-cwidth_opt = abap_true.
    mo_alv_parts->set_frontend_layout( is_layout = ls_layo_p ).
    data ls_stbl_p type lvc_s_stbl.
    ls_stbl_p-row = abap_true.
    ls_stbl_p-col = abap_true.
    mo_alv_parts->refresh_table_display( is_stable = ls_stbl_p ).
    mo_alv_parts->set_toolbar_interactive( ).
    mv_refreshing = abap_false.
  endmethod.
  method refresh_vers.
    check mv_refreshing = abap_false.
    mv_refreshing = abap_true.
    data ls_layo_v type lvc_s_layo.
    mo_alv_vers->get_frontend_layout( importing es_layout = ls_layo_v ).
    ls_layo_v-cwidth_opt = abap_true.
    mo_alv_vers->set_frontend_layout( is_layout = ls_layo_v ).
    data ls_stbl type lvc_s_stbl.
    ls_stbl-row = abap_true.
    ls_stbl-col = abap_true.
    mo_alv_vers->refresh_table_display( is_stable = ls_stbl ).
    mv_refreshing = abap_false.
  endmethod.
  method update_ver_colors.
    loop at mt_versions assigning field-symbol(<v>).
      if <v>-versno = ms_base_ver-versno.
        <v>-rowcolor = 'C510'.  " green background = base
      elseif <v>-versno = iv_viewed_versno and iv_viewed_versno <> ms_base_ver-versno.
        <v>-rowcolor = 'C710'.  " blue = currently viewed
      elseif <v>-trfunction = 'K' and <v>-task is not initial.
        <v>-rowcolor = 'C501'.  "  green = workbench request (type K)
      else.
        clear <v>-rowcolor.
      endif.
    endloop.
    refresh_vers( ).
  endmethod.
  method load_versions_task_view.
    clear mt_versions.

    " Use zcl_ave_vrsd — same source as TR view, includes Active/Modified
    try.
        data(lo_vrsd) = new zcl_ave_vrsd(
          type              = i_objtype
          name              = i_objname
          ignore_unreleased = abap_false
          no_toc            = abap_false ).
      catch zcx_ave.
        return.
    endtry.

    data lv_tv_trf_s type e070-trfunction value 'S'.

    loop at lo_vrsd->vrsd_list into data(ls_v).
      data ls_row type ty_version_row.
      ls_row-versno  = zcl_ave_versno=>to_external( ls_v-versno ).
      ls_row-versno_text = cond string(
        when ls_row-versno = zcl_ave_version=>c_version-active   then 'Active'
        when ls_row-versno = zcl_ave_version=>c_version-modified then 'Modified'
        else conv string( ls_row-versno + 0 ) ).
      ls_row-datum      = ls_v-datum.
      ls_row-zeit       = ls_v-zeit.
      ls_row-author     = ls_v-author.
      ls_row-korrnum    = ls_v-korrnum.
      ls_row-objtype    = i_objtype.
      ls_row-objname    = i_objname.
      if ls_v-korrnum is not initial.
        select single trfunction from e070
          where trkorr = @ls_v-korrnum into @ls_row-trfunction.
        select single as4text from e07t
          where trkorr = @ls_v-korrnum and langu = @sy-langu into @ls_row-korr_text.
      endif.
      ls_row-author_name = zcl_ave_popup_data=>get_user_name( ls_row-author ).
      append ls_row to mt_versions.
      clear ls_row.
    endloop.

    sort mt_versions by versno descending datum descending zeit descending.

    types: begin of ty_tv_obj_key,
             object   type e071-object,
             obj_name type e071-obj_name,
           end of ty_tv_obj_key.
    types: begin of ty_tv_task_cand,
             trkorr  type trkorr,
             strkorr type trkorr,
             as4user type as4user,
             as4date type as4date,
             as4time type as4time,
           end of ty_tv_task_cand.
    data lt_tv_keys      type sorted table of ty_tv_obj_key with unique key object obj_name.
    data lt_tv_all_tasks type standard table of ty_tv_task_cand.

    data lv_tv_e071_type type e071-object.
    data lv_tv_e071_name type versobjnam.
    lv_tv_e071_type = switch e071-object( i_objtype
      when 'REPS' or 'REPT'                                then 'PROG'
      when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' then 'CLAS'
      else i_objtype ).
    lv_tv_e071_name = i_objname.
    case i_objtype.
      when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' or 'REPT'.
        data(lv_tv_eq) = find( val = lv_tv_e071_name sub = '=' ).
        if lv_tv_eq > 0.
          lv_tv_e071_name = lv_tv_e071_name(lv_tv_eq).
        endif.
    endcase.

    insert value #( object = lv_tv_e071_type obj_name = lv_tv_e071_name ) into table lt_tv_keys.
    if lv_tv_e071_type = 'PROG'.
      insert value #( object = 'REPS' obj_name = lv_tv_e071_name ) into table lt_tv_keys.
    elseif lv_tv_e071_type = 'REPS'.
      insert value #( object = 'PROG' obj_name = lv_tv_e071_name ) into table lt_tv_keys.
    endif.

    select e070~trkorr, e070~strkorr, e070~as4user, e070~as4date, e070~as4time
      from e071
      inner join e070 on e070~trkorr = e071~trkorr
      for all entries in @lt_tv_keys
      where e071~object     = @lt_tv_keys-object
        and e071~obj_name   = @lt_tv_keys-obj_name
        and e070~trfunction = @lv_tv_trf_s
      into table @lt_tv_all_tasks.
    sort lt_tv_all_tasks by as4date descending as4time descending.

    loop at mt_versions assigning field-symbol(<ver>).
      loop at lt_tv_all_tasks into data(ls_cand).
        check ls_cand-as4date < <ver>-datum
           or ( ls_cand-as4date = <ver>-datum and ls_cand-as4time <= <ver>-zeit ).
        if <ver>-trfunction = 'K' and ls_cand-strkorr <> <ver>-korrnum.
          continue.
        endif.
        <ver>-task        = ls_cand-trkorr.
        <ver>-author      = ls_cand-as4user.
        <ver>-author_name = zcl_ave_popup_data=>get_user_name( ls_cand-as4user ).
        exit.
      endloop.
    endloop.

    if mv_remove_dup = abap_true.
      zcl_ave_popup_data=>remove_duplicate_versions(
        exporting
          i_keep_korrnum = cond #( when mv_object_type = zcl_ave_object_factory=>gc_type-tr
                                   then conv trkorr( mv_object_name ) )
          i_ignore_case  = mv_ignore_case
        changing
          ct_versions    = mt_versions ).
    endif.

    if mv_no_toc = abap_true.
      delete mt_versions where trfunction = 'T'.
    endif.
  endmethod.
  method handle_vers_toolbar.
    clear e_object->mt_toolbar.
    append value stb_button(
      function  = 'DIFF_MODE_TOGGLE'
      icon      = conv #( icon_compare )
      text      = cond #( when mv_diff_prev = abap_true then 'Diff prev' else 'Diff any' )
      quickinfo = 'Switch diff mode: compare with previous or any base'
      butn_type = 0 ) to e_object->mt_toolbar.
    append value stb_button( butn_type = 3 ) to e_object->mt_toolbar. " separator
    if mv_diff_prev = abap_false.
      append value stb_button(
        function  = 'SET_BASE'
        icon      = conv #( icon_header )
        text      = 'Set Base'
        quickinfo = 'Set selected version as base'
        butn_type = 0 ) to e_object->mt_toolbar.
    endif.
    append value stb_button( butn_type = 3 ) to e_object->mt_toolbar. " separator
    append value stb_button(
      function  = 'TOC_TOGGLE'
      icon      = conv #( icon_list )
      text      = cond #( when mv_no_toc = abap_true then 'TOCs off' else 'TOCs on' )
      quickinfo = 'Toggle TOC versions'
      butn_type = 0 ) to e_object->mt_toolbar.
    append value stb_button(
      function  = 'DUP_TOGGLE'
      icon      = conv #( icon_overview )
      text      = cond #( when mv_remove_dup = abap_true then 'Dups off' else 'Dups on' )
      quickinfo = 'Toggle duplicate versions'
      butn_type = 0 ) to e_object->mt_toolbar.
    append value stb_button( butn_type = 3 ) to e_object->mt_toolbar. " separator
    append value stb_button(
      function  = 'CASE_TOGGLE'
      icon      = conv #( icon_abc )
      text      = cond #( when mv_ignore_case = abap_true then 'Case off' else 'Case on' )
      quickinfo = 'Toggle case-insensitive diff'
      butn_type = 0 ) to e_object->mt_toolbar.
  endmethod.
  method handle_vers_command.
    case e_ucomm.
      when 'DIFF_MODE_TOGGLE'.
        mv_diff_prev = cond #( when mv_diff_prev = abap_true then abap_false else abap_true ).
        refresh_vers( ).

      when 'TOC_TOGGLE'.
        mv_no_toc = cond #( when mv_no_toc = abap_true then abap_false else abap_true ).
        load_versions( i_objtype = mv_cur_objtype i_objname = mv_cur_objname ).
        refresh_vers( ).

      when 'DUP_TOGGLE'.
        mv_remove_dup = cond #( when mv_remove_dup = abap_true then abap_false else abap_true ).
        load_versions( i_objtype = mv_cur_objtype i_objname = mv_cur_objname ).
        refresh_vers( ).

      when 'CASE_TOGGLE'.
        mv_ignore_case = cond #( when mv_ignore_case = abap_true then abap_false else abap_true ).
        if mv_remove_dup = abap_true.
          load_versions( i_objtype = mv_cur_objtype i_objname = mv_cur_objname ).
        endif.
        refresh_vers( ).
        if mv_show_diff = abap_true and ms_diff_old is not initial.
          show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
        endif.

      when 'SET_BASE'.
        data lt_rows type lvc_t_row.
        mo_alv_vers->get_selected_rows( importing et_index_rows = lt_rows ).
        check lines( lt_rows ) = 1.
        ms_base_ver = mt_versions[ lt_rows[ 1 ]-index ].
        if mv_viewed_versno is not initial and mv_show_diff = abap_true.
          read table mt_versions into data(ls_viewed) with key versno = mv_viewed_versno.
          if sy-subrc = 0.
            show_versions_diff( is_old = ls_viewed is_new = ms_base_ver ).
          endif.
        endif.
        update_ver_colors( iv_viewed_versno = mv_viewed_versno ).

      when others.
        on_toolbar_click( fcode = e_ucomm ).
    endcase.
  endmethod.
  method handle_vers_dblclick.
    data(lv_row) = es_row_no-row_id.
    read table mt_versions into data(ls_ver) index lv_row.
    if sy-subrc <> 0. return. endif.

    mv_viewed_versno = ls_ver-versno.

    if mv_show_diff = abap_true.
      if mv_diff_prev = abap_true.
        " Diff prev mode: clicked = new, next in list = old (previous chronologically)
        read table mt_versions into data(ls_prev) index lv_row + 1.
        ms_base_ver = ls_ver.
        " No previous version → show as new object (all-green diff vs empty source)
        show_versions_diff( is_old = ls_prev is_new = ls_ver ).
      else.
        " Diff any mode: compare with manually chosen base
        if ls_ver-versno = ms_base_ver-versno.
          read table mt_versions into data(ls_prev_base) index lv_row + 1.
          " No previous version → show as new object
          show_versions_diff( is_old = ls_prev_base is_new = ls_ver ).
        else.
          show_versions_diff( is_old = ls_ver is_new = ms_base_ver ).
        endif.
      endif.
    else.
      show_source( i_objtype = ls_ver-objtype i_objname = ls_ver-objname i_versno = ls_ver-versno ).
    endif.

    update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
  endmethod.
  method show_source.
    if mo_box is bound.
      data lv_vtxt type string.
      read table mt_versions into data(ls_vcap) with key versno = i_versno.
      lv_vtxt = cond #( when sy-subrc = 0 then ls_vcap-versno_text else conv string( i_versno ) ).
      data(lv_vlbl) = cond string( when lv_vtxt ca '0123456789' and lv_vtxt na 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                   then |v{ lv_vtxt }| else lv_vtxt ).
      data(lv_extra) = cond string(
        when mv_cur_part_name is not initial
        then | – { mv_cur_part_name }|
        when i_objname is not initial and i_objname <> mv_object_name
        then | – { i_objtype }: { i_objname }|
        else `` ).
      mo_box->set_caption( |{ mv_object_type }: { mv_object_name }{ lv_extra }  [{ lv_vlbl }]| ).
    endif.
    try.
        " Find VRSD row for this version
        data lt_vrsd type vrsd_tab.
        data(lv_db_versno) = zcl_ave_versno=>to_internal( i_versno ).
        select * from vrsd
          where objtype = @i_objtype
            and objname = @i_objname
            and versno  = @lv_db_versno
            into table @lt_vrsd
          up to 1 rows.

        data ls_vrsd type vrsd.
        if lt_vrsd is not initial.
          ls_vrsd = lt_vrsd[ 1 ].
        else.
          " Active/Modified: get timestamp from already-loaded version data
          ls_vrsd-objtype = i_objtype.
          ls_vrsd-objname = i_objname.
          ls_vrsd-versno  = lv_db_versno.
          read table mt_versions into data(ls_ver_row)
            with key versno = i_versno objtype = i_objtype objname = i_objname.
          if sy-subrc = 0.
            ls_vrsd-author = ls_ver_row-author.
            ls_vrsd-datum  = ls_ver_row-datum.
            ls_vrsd-zeit   = ls_ver_row-zeit.
          else.
            ls_vrsd-author = sy-uname.
          endif.
        endif.

        data(lo_ver)    = new zcl_ave_version( ls_vrsd ).
        data(lt_source) = lo_ver->get_source( ).

        " ABAP editor handles 100k+ line sources much faster than HTML.
        " Version metadata stays visible in the dialog caption + version list.
        show_code_source( it_source = lt_source ).
*        IF i_objtype = 'DDLS'.
*          set_html( zcl_ave_popup_html=>cds_source_to_html(
*            it_source = lt_source
*            i_title   = |{ i_objtype }: { i_objname }|
*            i_meta    = lv_vlbl ) ).
*        ELSE.
*          show_code_source( it_source = lt_source ).
*        ENDIF.

      catch zcx_ave.
        set_html(
          |<html><body style="background:#1e1e1e;color:#f55;| &&
          |font-family:Consolas;padding:20px">| &&
          |Error loading source.</body></html>| ).
    endtry.
  endmethod.
  method show_code_source.
    if mo_code_viewer is bound.
      data lt_src type standard table of char255.
      loop at it_source into data(ls_line).
        append conv char255( ls_line ) to lt_src.
      endloop.
      mo_code_viewer->set_text( table = lt_src ).
      mo_code_viewer->set_readonly_mode( 1 ).
      if mo_split_html is bound.
        mo_split_html->set_row_height( id = 1 height = 0 ).
        mo_split_html->set_row_height( id = 2 height = 100 ).
      endif.
      cl_gui_cfw=>flush( ).
    endif.
  endmethod.
  method set_html.
    mv_last_html = iv_html.
    " Previous call may have swapped to the ABAP editor — bring HTML back.
    if mo_split_html is bound.
      mo_split_html->set_row_height( id = 1 height = 100 ).
      mo_split_html->set_row_height( id = 2 height = 0 ).
    endif.
    data: lt_html   type w3htmltab,
          lv_url    type w3url,
          lv_offset type i,
          lv_len    type i,
          lv_chunk  type i.

    lv_len = strlen( iv_html ).
    while lv_offset < lv_len.
      lv_chunk = cond #(
        when lv_len - lv_offset > 255 then 255
        else lv_len - lv_offset ).
      append value #( line = iv_html+lv_offset(lv_chunk) ) to lt_html.
      lv_offset += lv_chunk.
    endwhile.

    mo_html->load_data(
      importing
        assigned_url = lv_url
      changing
        data_table   = lt_html
      exceptions
        others       = 1 ).

    mo_html->show_url( url = lv_url ).
    cl_gui_cfw=>flush( ).
  endmethod.
  method get_class_parts.
    data(lo_obj) = new zcl_ave_object_factory( )->get_instance(
      object_type = zcl_ave_object_factory=>gc_type-class
      object_name = conv #( i_name ) ).

    loop at lo_obj->get_parts( ) into data(ls_part).
      check ls_part-type <> 'CLSD' and ls_part-type <> 'RELE'.
      if ls_part-type <> 'METH'.
        check zcl_ave_popup_data=>check_part_exists(
                     i_type       = ls_part-type
                     i_name       = conv #( ls_part-object_name ) ).

      endif.
      data ls_part_row type ty_part_row.
      clear ls_part_row.
      ls_part_row-class       = ls_part-class.
      ls_part_row-name        = ls_part-unit.
      ls_part_row-type        = ls_part-type.
      ls_part_row-type_text   = zcl_ave_popup_data=>get_type_text( ls_part-type ).
      ls_part_row-object_name = ls_part-object_name.
      ls_part_row-exists_flag = abap_true.
      ls_part_row-rows = zcl_ave_popup_data=>get_active_line_count(
        i_type = ls_part-type
        i_name = ls_part-object_name ).
      " TR drill-down: color if changed vs prior K-TR (author irrelevant).
      if zcl_ave_popup_data=>is_substantive_user_change(
           it_versions = zcl_ave_popup_data=>build_versions_for_check( i_type = ls_part-type i_name = ls_part-object_name )
           i_type      = ls_part-type
           i_name      = ls_part-object_name
           i_korrnum   = cond #( when mv_object_type = zcl_ave_object_factory=>gc_type-tr
                                  then conv verskorrno( mv_object_name ) ) ) = abap_true.
        ls_part_row-rowcolor = 'C510'. " green
      endif.
      append ls_part_row to result.
    endloop.
  endmethod.
  method on_toolbar_click.
    case fcode.
      when 'SAVE_REVIEW'.
        if has_review_table( ) = abap_false.
          show_review_help_popup( ).
        else.
          save_review_to_db( ).
        endif.

      when 'INFO'.
        data(l_url) = 'https://github.com/ysichov/AVE'.
        call function 'CALL_BROWSER' exporting url = l_url.

      when 'BACK'.
        check mt_parts_backup is not initial.
        mt_parts = mt_parts_backup.
        clear: mt_parts_backup, mv_drilled_class.
        refresh_parts( ).

      when 'REFRESH'.
        if mv_code_review = abap_true.
          load_review_from_db( ).
          regen_acr_report( ).
          refresh_rpt_row( ).

          if mv_decline_view_user is not initial.
            show_user_declines( iv_user = mv_decline_view_user iv_reviewer = mv_reviewer_view ).
          elseif mv_cr_base_html is not initial and mv_cr_cur_key is not initial.
            set_html( inject_approve_btn( iv_html = mv_cr_base_html iv_key = mv_cr_cur_key ) ).
          else.
            set_html( mv_cr_report_html ).
          endif.
          return.
        endif.

        " Reload parts
        clear mt_parts.
        try.
            if mv_drilled_class is not initial.
              " Drilled into a class from a TR parts view — refresh only this class.
              mt_parts = get_class_parts( conv #( mv_drilled_class ) ).
            elseif mv_object_type = zcl_ave_object_factory=>gc_type-class.
              mt_parts = get_class_parts( conv #( mv_object_name ) ).
            else.
              data(lo_obj) = new zcl_ave_object_factory( )->get_instance(
                object_type = mv_object_type
                object_name = conv #( mv_object_name ) ).
              data(lv_is_tr) = boolc( mv_object_type = zcl_ave_object_factory=>gc_type-tr ).
              loop at lo_obj->get_parts( ) into data(ls_raw).
                data(lv_exists) = cond abap_bool(
                  when lv_is_tr = abap_true
                  then zcl_ave_popup_data=>check_part_exists(
                         i_type       = ls_raw-type
                         i_name       = ls_raw-object_name
                         i_class_name = conv #( ls_raw-class ) )
                  else abap_true ).
                data ls_row type ty_part_row.
                ls_row-class       = ls_raw-class.
                ls_row-name        = ls_raw-unit.
                ls_row-type        = ls_raw-type.
                ls_row-type_text   = zcl_ave_popup_data=>get_type_text( ls_raw-type ).
                ls_row-object_name = ls_raw-object_name.
                ls_row-exists_flag = lv_exists.
                ls_row-rows        = cond i( when lv_exists = abap_true
                  then zcl_ave_popup_data=>get_active_line_count( i_type = ls_raw-type i_name = ls_raw-object_name )
                  else 0 ).
                if lv_exists = abap_false.
                  ls_row-rowcolor = 'C601'.   " red
                else.
                  data(lv_changed2) = cond abap_bool(
                    when ls_raw-type = 'CLAS'
                    then zcl_ave_popup_data=>check_class_has_author(
                           i_class_name = conv #( ls_raw-object_name )
                           i_korrnum    = cond #( when lv_is_tr = abap_true then conv verskorrno( mv_object_name ) ) )
                    else zcl_ave_popup_data=>is_substantive_user_change(
                           it_versions = zcl_ave_popup_data=>build_versions_for_check( i_type = ls_raw-type i_name = ls_raw-object_name )
                           i_type      = ls_raw-type
                           i_name      = ls_raw-object_name
                           i_korrnum   = cond #( when lv_is_tr = abap_true then conv verskorrno( mv_object_name ) ) ) ).
                  if lv_changed2 = abap_true.
                    ls_row-rowcolor = 'C510'. " green
                  endif.
                endif.
                append ls_row to mt_parts.
                clear ls_row.
              endloop.
            endif.
          catch zcx_ave.
        endtry.
        refresh_parts( ).
        clear mt_diff_cache.
        " Reload versions for current part if one was selected
        if mv_cur_objtype is not initial.
          load_versions( i_objtype = mv_cur_objtype i_objname = mv_cur_objname ).
          update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
        endif.
        " Re-render diff if it was already open (cache cleared above forces fresh render)
        if ms_diff_old is not initial and ms_diff_new is not initial.
          show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
        endif.

      when 'SET_BASE'.
        data lt_sel_base type lvc_t_row.
        mo_alv_vers->get_selected_rows( importing et_index_rows = lt_sel_base ).
        check lines( lt_sel_base ) = 1.
        ms_base_ver = mt_versions[ lt_sel_base[ 1 ]-index ].
        if mv_viewed_versno is not initial and mv_show_diff = abap_true.
          read table mt_versions into data(ls_viewed) with key versno = mv_viewed_versno.
          if sy-subrc = 0.
            show_versions_diff( is_old = ls_viewed is_new = ms_base_ver ).
          endif.
        endif.
        update_ver_colors( iv_viewed_versno = mv_viewed_versno ).

      when 'DIFF_TOGGLE'.
        mv_show_diff = cond #( when mv_show_diff = abap_true then abap_false else abap_true ).
        mo_toolbar->set_button_info(
          exporting
            fcode = 'DIFF_TOGGLE'
            text  = cond #( when mv_show_diff = abap_true
                            then 'Show Diff' else 'Show Vers' )
            icon  = cond #( when mv_show_diff = abap_true
                            then icon_compare else icon_history ) ).
        if mv_viewed_versno is not initial.
          read table mt_versions into data(ls_vw) with key versno = mv_viewed_versno.
          if sy-subrc = 0.
            if mv_show_diff = abap_true.
              " Restore last diff pair (ms_diff_old/new set by show_versions_diff)
              if ms_diff_old is not initial or ms_diff_new is not initial.
                show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
              else.
                show_versions_diff( is_old = ls_vw is_new = ms_base_ver ).
              endif.
            else.
              show_source( i_objtype = ls_vw-objtype i_objname = ls_vw-objname i_versno = ls_vw-versno ).
            endif.
          endif.
        endif.

      when 'PANE_TOGGLE'.
        mv_two_pane = cond #( when mv_two_pane = abap_true then abap_false else abap_true ).
        mo_toolbar->set_button_info(
          exporting
            fcode = 'PANE_TOGGLE'
            text  = cond #( when mv_two_pane = abap_true
                            then '2-Pane' else 'Inline' )
            icon  = cond #( when mv_two_pane = abap_true
                            then icon_view_hier_list else icon_spool_request ) ).
        if rerender_cr_user_view( ) = abap_true.
          return.
        endif.
        if rerender_cr_current( ) = abap_true.
          return.
        endif.
        if mv_viewed_versno is not initial and mt_versions is not initial.
          read table mt_versions into data(ls_pv) with key versno = mv_viewed_versno.
          if sy-subrc = 0.
            if mv_show_diff = abap_true.
              if ms_diff_old is not initial or ms_diff_new is not initial.
                show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
              else.
                show_versions_diff( is_old = ls_pv is_new = ms_base_ver ).
              endif.
            else.
              show_source( i_objtype = ls_pv-objtype i_objname = ls_pv-objname i_versno = ls_pv-versno ).
            endif.
          endif.
        endif.

      when 'COMPACT_TOGGLE'.
        mv_compact = cond #( when mv_compact = abap_true then abap_false else abap_true ).
        mo_toolbar->set_button_info(
          exporting
            fcode = 'COMPACT_TOGGLE'
            text  = cond #( when mv_compact = abap_true then 'Compact' else 'Full' )
            icon  = cond #( when mv_compact = abap_true
                            then icon_collapse_all else icon_expand_all ) ).
        if rerender_cr_user_view( ) = abap_true.
          return.
        endif.
        if rerender_cr_current( ) = abap_true.
          return.
        endif.
        if mv_show_diff = abap_true and ms_diff_old is not initial.
          show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
        endif.

      when 'BLAME_TOGGLE'.
        mv_blame = cond #( when mv_blame = abap_true then abap_false else abap_true ).
        mo_toolbar->set_button_info(
          exporting
            fcode = 'BLAME_TOGGLE'
            text  = cond #( when mv_blame = abap_true then 'Blame ON' else 'Blame' )
            icon  = conv #( icon_history ) ).
        if rerender_cr_user_view( ) = abap_true.
          return.
        endif.
        if rerender_cr_current( ) = abap_true.
          return.
        endif.
        if mv_show_diff = abap_true and ms_diff_old is not initial.
          show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
        endif.

      when 'DEBUG'.
        mv_debug = cond #( when mv_debug = abap_true then abap_false else abap_true ).
        mo_toolbar->set_button_info(
          exporting
            fcode = 'DEBUG'
            text  = cond #( when mv_debug = abap_true then 'Debug ON' else 'Debug' )
            icon  = conv #( icon_bw_dm_aa ) ).
        if rerender_cr_user_view( ) = abap_true.
          return.
        endif.
        if rerender_cr_current( ) = abap_true.
          return.
        endif.
        " Re-render the current diff (if any) using the new mode
        if mv_show_diff = abap_true and ms_diff_old is not initial.
          show_versions_diff( is_old = ms_diff_old is_new = ms_diff_new ).
        endif.

      when 'FOCUS_TOGGLE'.
        if mv_focus_html = abap_true.
          " currently maximized → restore
          mv_focus_html = abap_false.
          mo_toolbar->set_button_info(
            exporting
              fcode = 'FOCUS_TOGGLE'
              text  = 'Maximize View'
              icon  = conv #( icon_view_maximize ) ).
          if mv_two_pane = abap_true.
            mo_split_2p_wrap->set_row_height( id = 1 height = 35 ).
            mo_split_2p_wrap->set_row_height( id = 2 height = 65 ).
            mo_split_2p_wrap->set_row_sash( id = 1 type = 1 value = 0 ).
          else.
            mo_split_main->set_column_width( id = 1 width = 40 ).
            mo_split_main->set_column_width( id = 2 width = 60 ).
            mo_split_main->set_column_sash( id = 1 type = 1 value = 0 ).
          endif.
        else.
          maximize_html( ).
        endif.

    endcase.
  endmethod.
  method on_box_close.
    sender->free( ).
    clear mo_box.
  endmethod.
  method on_help_box_close.
    sender->free( ).
    clear: mo_help_box, mo_help_html.
  endmethod.
  method has_review_table.

    select single tabname
      from dd02l
      where tabname  = 'ZAVE_REVIEW'
        and as4local = 'A'
        and tabclass = 'TRANSP'
      into @data(lv_tabname).

    result = xsdbool( sy-subrc = 0 and lv_tabname is not initial ).
  endmethod.
  method load_review_payload.
    clear es_payload.
    data lv_payload_json type string.
    data lv_tabname type tabname value 'ZAVE_REVIEW'.

    try.
        select single payload

          from (lv_tabname)
          where trkorr = @iv_trkorr
          into @lv_payload_json.
      catch cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_syntax
            cx_sy_open_sql_db.
        return.
    endtry.

    if sy-subrc <> 0 or lv_payload_json is initial.
      return.
    endif.

    try.
        /ui2/cl_json=>deserialize(
          exporting
            json = lv_payload_json
          changing
            data = es_payload ).
        result = abap_true.
      catch cx_root.
        clear es_payload.
    endtry.
  endmethod.
  method load_review_from_db.
    check mv_code_review = abap_true.
    check mv_object_type = zcl_ave_object_factory=>gc_type-tr.
    check has_review_table( ) = abap_true.

    clear: mt_approved, mt_declined, mt_decline_notes, mt_hunk_threads, mt_hunk_actions.

    data(ls_payload) = value ty_saved_payload( ).
    check load_review_payload(
      exporting iv_trkorr = conv #( mv_object_name )
      importing es_payload = ls_payload ) = abap_true.

    if mt_acr_stats is initial and ls_payload-obj_stats is not initial.
      mt_acr_stats = ls_payload-obj_stats.
    endif.
    if mt_hunk_info is initial and ls_payload-hunks is not initial.
      mt_hunk_info = ls_payload-hunks.
    endif.
    if mt_diff_cache is initial and ls_payload-diff_cache is not initial.
      mt_diff_cache = ls_payload-diff_cache.
    endif.
    mt_hunk_actions = ls_payload-hunk_actions.

    loop at ls_payload-threads into data(ls_saved_thread).
      data(ls_thread) = value ty_hunk_thread(
        hunk_key     = ls_saved_thread-hunk_key
        objtype      = ls_saved_thread-objtype
        obj_name     = ls_saved_thread-obj_name
        class_name   = ls_saved_thread-class_name
        display_name = ls_saved_thread-display_name
        hunk_no      = ls_saved_thread-hunk_no
        start_line   = ls_saved_thread-start_line
        change_count = ls_saved_thread-change_count
        change_kind  = ls_saved_thread-change_kind
        html         = ls_saved_thread-html
        messages     = ls_saved_thread-messages ).
      read table mt_hunk_info into data(ls_hunk_info_cur)
        with table key hunk_key = ls_saved_thread-hunk_key.
      if sy-subrc = 0.
        ls_thread-objtype      = ls_hunk_info_cur-objtype.
        ls_thread-obj_name     = ls_hunk_info_cur-obj_name.
        ls_thread-class_name   = ls_hunk_info_cur-class_name.
        ls_thread-display_name = ls_hunk_info_cur-display_name.
        ls_thread-hunk_no      = ls_hunk_info_cur-hunk_no.
        ls_thread-start_line   = ls_hunk_info_cur-start_line.
        ls_thread-change_count = ls_hunk_info_cur-change_count.
        ls_thread-change_kind  = ls_hunk_info_cur-change_kind.
        ls_thread-html         = ls_hunk_info_cur-html.
      endif.
      if not line_exists( mt_hunk_info[ hunk_key = ls_saved_thread-hunk_key ] ).
        insert value ty_hunk_info(
          hunk_key     = ls_saved_thread-hunk_key
          objtype      = ls_saved_thread-objtype
          obj_name     = ls_saved_thread-obj_name
          class_name   = ls_saved_thread-class_name
          display_name = ls_saved_thread-display_name
          hunk_no      = ls_saved_thread-hunk_no
          start_line   = ls_saved_thread-start_line
          change_count = ls_saved_thread-change_count
          change_kind  = ls_saved_thread-change_kind
          author       = ls_saved_thread-author
          author_name  = ls_saved_thread-author_name
          html         = ls_saved_thread-html ) into table mt_hunk_info.
      endif.
      insert ls_thread into table mt_hunk_threads.
    endloop.

    loop at ls_payload-user_states into data(ls_action_state).
      loop at ls_action_state-approved into data(ls_action_approved).
        if ( mt_hunk_info is initial
             or line_exists( mt_hunk_info[ hunk_key = ls_action_approved-hunk_key ] ) )
           and not line_exists( mt_hunk_actions[
             hunk_key = ls_action_approved-hunk_key reviewer = ls_action_state-reviewer action = 'A' ] ).
          append value ty_hunk_action(
            hunk_key      = ls_action_approved-hunk_key
            reviewer      = ls_action_state-reviewer
            reviewer_name = ls_action_state-reviewer_name
            action        = 'A'
            changed_at    = ls_action_state-saved_at ) to mt_hunk_actions.
        endif.
      endloop.
      loop at ls_action_state-declined into data(ls_action_declined).
        if ( mt_hunk_info is initial
             or line_exists( mt_hunk_info[ hunk_key = ls_action_declined-hunk_key ] ) )
           and not line_exists( mt_hunk_actions[
             hunk_key = ls_action_declined-hunk_key reviewer = ls_action_state-reviewer action = 'D' ] ).
          append value ty_hunk_action(
            hunk_key      = ls_action_declined-hunk_key
            reviewer      = ls_action_state-reviewer
            reviewer_name = ls_action_state-reviewer_name
            action        = 'D'
            changed_at    = ls_action_state-saved_at ) to mt_hunk_actions.
        endif.
      endloop.
    endloop.

    read table ls_payload-user_states into data(ls_user_state)
      with key reviewer = sy-uname.
    if sy-subrc = 0.
      loop at ls_user_state-approved into data(ls_approved_key).
        insert ls_approved_key-hunk_key into table mt_approved.
        if not line_exists( mt_hunk_actions[
          hunk_key = ls_approved_key-hunk_key reviewer = ls_user_state-reviewer action = 'A' ] ).
          append value ty_hunk_action(
            hunk_key      = ls_approved_key-hunk_key
            reviewer      = ls_user_state-reviewer
            reviewer_name = ls_user_state-reviewer_name
            action        = 'A'
            changed_at    = ls_user_state-saved_at ) to mt_hunk_actions.
        endif.
      endloop.
      loop at ls_user_state-declined into data(ls_declined_key).
        insert ls_declined_key-hunk_key into table mt_declined.
        if not line_exists( mt_hunk_actions[
          hunk_key = ls_declined_key-hunk_key reviewer = ls_user_state-reviewer action = 'D' ] ).
          append value ty_hunk_action(
            hunk_key      = ls_declined_key-hunk_key
            reviewer      = ls_user_state-reviewer
            reviewer_name = ls_user_state-reviewer_name
            action        = 'D'
            changed_at    = ls_user_state-saved_at ) to mt_hunk_actions.
        endif.
      endloop.
      loop at ls_user_state-notes into data(ls_saved_note).
        insert value ty_decline_note(
          hunk_key = ls_saved_note-hunk_key
          note     = ls_saved_note-note ) into table mt_decline_notes.
      endloop.
    endif.

    sanitize_review_state( ).
  endmethod.
  method render_decline_thread_html.
    read table mt_hunk_threads into data(ls_thread)
      with table key hunk_key = iv_hunk_key.
    if sy-subrc <> 0.
      read table mt_decline_notes into data(ls_note)
        with table key hunk_key = iv_hunk_key.
      if sy-subrc = 0 and ls_note-note is not initial.
        data(lv_note_esc) = ls_note-note.
        replace all occurrences of `&` in lv_note_esc with `&amp;`.
        replace all occurrences of `<` in lv_note_esc with `&lt;`.
        replace all occurrences of `>` in lv_note_esc with `&gt;`.
        replace all occurrences of cl_abap_char_utilities=>newline in lv_note_esc with `<br>`.
        data(lv_note_bg) = cond string(
          when line_exists( mt_declined[ table_line = iv_hunk_key ] ) then `#fff1f4`
          else `#f3f9ff` ).
        data(lv_note_border) = cond string(
          when line_exists( mt_declined[ table_line = iv_hunk_key ] ) then `#efb8c8`
          else `#a8cde8` ).
        data(lv_note_text) = cond string(
          when line_exists( mt_declined[ table_line = iv_hunk_key ] ) then `#9f3b57`
          else `#2874a6` ).
        result =
          `<tr><td class="ln">&nbsp;</td><td class="cd" style="padding:6px 12px">` &&
          `<div style="display:inline-block;background:` && lv_note_bg &&
          `;border:1px solid ` && lv_note_border &&
          `;padding:5px 9px;color:` && lv_note_text &&
          `;font-size:11px;line-height:15px;font-style:italic;border-radius:6px">` &&
          lv_note_esc && `</div></td></tr>`.
      endif.
      return.
    endif.

    loop at ls_thread-messages into data(ls_msg).
      data(lv_author_esc) = escape( val = conv string( ls_msg-author ) format = cl_abap_format=>e_html_text ).
      data(lv_author_name_esc) = escape( val = conv string( ls_msg-author_name ) format = cl_abap_format=>e_html_text ).
      data(lv_created_at_txt) = format_timestamp( ls_msg-created_at ).
      data(lv_text_esc) = escape( val = ls_msg-text format = cl_abap_format=>e_html_text ).
      replace all occurrences of cl_abap_char_utilities=>newline in lv_text_esc with `<br>`.
      data(lv_note_bg_msg) = cond string(
        when ls_msg-is_decline = abap_true then `#fff1f4`
        else `#f3f9ff` ).
      data(lv_note_border_msg) = cond string(
        when ls_msg-is_decline = abap_true then `#efb8c8`
        else `#a8cde8` ).
      data(lv_note_text_msg) = cond string(
        when ls_msg-is_decline = abap_true then `#9f3b57`
        else `#2874a6` ).
      result = result &&
        `<tr><td class="ln">&nbsp;</td><td class="cd" style="padding:6px 12px">` &&
        `<div style="display:inline-block;margin:0 0 6px 0;background:` && lv_note_bg_msg &&
        `;border:1px solid ` && lv_note_border_msg && `;padding:6px 9px;max-width:900px;border-radius:6px">` &&
        `<div style="font-size:10px;color:#6f7f8f;font-weight:bold;margin-bottom:3px">` &&
        lv_author_esc && ` / ` && lv_author_name_esc &&
        ` <span style="font-weight:normal;color:#8a96a3">/ ` &&
        escape( val = lv_created_at_txt format = cl_abap_format=>e_html_text ) &&
        `</span></div>` &&
        `<div style="font-size:11px;line-height:15px;color:` && lv_note_text_msg &&
        `;font-style:italic">` &&
        lv_text_esc && `</div></div></td></tr>`.
    endloop.
  endmethod.
  method render_hunk_actions_html.
    data(lv_status_html) = ``.
    data(lv_actions_html) = ``.
    data(lv_own_hunk) = is_own_hunk( iv_hunk_key ).
    data(lv_global_action) = get_hunk_global_action( iv_hunk_key ).

    if line_exists( mt_approved[ table_line = iv_hunk_key ] ).
      lv_status_html =
        `<span style="color:#27ae60;font-weight:bold">&#10003; approved</span>` &&
        render_hunk_action_meta( iv_hunk_key = iv_hunk_key iv_action = 'A' ).
      if lv_own_hunk = abap_true.
        lv_actions_html = render_comment_links( iv_hunk_key ).
      else.
        lv_actions_html =
          |<a href="sapevent:undo~{ iv_hunk_key }"| &&
          ` style="margin-left:8px;background:#95a5a6;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">Undo</a>` &&
          render_comment_links( iv_hunk_key ).
      endif.
    elseif line_exists( mt_declined[ table_line = iv_hunk_key ] ).
      lv_status_html =
        `<span style="color:#e74c3c;font-weight:bold">&#10007; declined</span>` &&
        render_hunk_action_meta( iv_hunk_key = iv_hunk_key iv_action = 'D' ).
      if lv_own_hunk = abap_true.
        lv_actions_html = render_comment_links( iv_hunk_key ).
      else.
        lv_actions_html =
          |<a href="sapevent:undo~{ iv_hunk_key }"| &&
          ` style="margin-left:8px;background:#95a5a6;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">Undo</a>` &&
          |<a href="sapevent:approve~{ iv_hunk_key }"| &&
          ` style="margin-left:4px;background:#27ae60;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">&#10003; Approve</a>` &&
          render_comment_links( iv_hunk_key ).
      endif.
    elseif lv_global_action = 'A' or lv_global_action = 'D'.
      lv_status_html = cond string(
        when lv_global_action = 'A'
        then `<span style="color:#27ae60;font-weight:bold">&#10003; approved</span>` &&
             render_hunk_action_meta( iv_hunk_key = iv_hunk_key iv_action = 'A' )
        else `<span style="color:#e74c3c;font-weight:bold">&#10007; declined</span>` &&
             render_hunk_action_meta( iv_hunk_key = iv_hunk_key iv_action = 'D' ) ).
      if lv_own_hunk = abap_true.
        lv_actions_html = render_comment_links( iv_hunk_key ).
      else.
        lv_actions_html =
          |<a href="sapevent:approve~{ iv_hunk_key }"| &&
          ` style="margin-left:8px;background:#27ae60;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">&#10003; Approve</a>` &&
          |<a href="sapevent:decline~{ iv_hunk_key }"| &&
          ` style="margin-left:4px;background:#922b21;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">&#10007; Decline</a>` &&
          render_comment_links( iv_hunk_key ).
      endif.
    else.
      if lv_own_hunk = abap_true.
        lv_status_html =
          `<span style="color:#7f8c8d;font-weight:bold">&#9675; own block</span>`.
        lv_actions_html = render_comment_links( iv_hunk_key ).
      else.
        lv_status_html =
          `<span style="color:#7f8c8d;font-weight:bold">&#9675; open</span>`.
        lv_actions_html =
          |<a href="sapevent:approve~{ iv_hunk_key }"| &&
          ` style="margin-left:8px;background:#27ae60;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">&#10003; Approve</a>` &&
          |<a href="sapevent:decline~{ iv_hunk_key }"| &&
          ` style="margin-left:4px;background:#922b21;color:#fff;font-weight:bold;` &&
          `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">&#10007; Decline</a>` &&
          render_comment_links( iv_hunk_key ).
      endif.
    endif.

    result =
      `<div style="display:flex;align-items:center;gap:0;margin:2px 0 8px 0">` &&
      lv_status_html && lv_actions_html && `</div>`.
  endmethod.
  method render_comment_links.
    data(lv_last_note) = get_last_own_comment( iv_hunk_key ).
    if lv_last_note is not initial.
      result =
        |<a href="sapevent:editreview~{ iv_hunk_key }"| &&
        ` style="margin-left:4px;background:#7f8c8d;color:#fff;font-weight:bold;` &&
        `text-decoration:none;font-style:normal;font-size:11px;` &&
        `border-radius:3px;padding:2px 7px">Edit</a>`.
    endif.

    result = result &&
      |<a href="sapevent:addcomment~{ iv_hunk_key }"| &&
      ` style="margin-left:4px;background:#3498db;color:#fff;font-weight:bold;` &&
      `text-decoration:none;font-style:normal;font-size:11px;` &&
      `border-radius:3px;padding:2px 7px">Add Comment</a>`.
  endmethod.
  method get_last_own_comment.
    read table mt_hunk_threads into data(ls_thread)
      with table key hunk_key = iv_hunk_key.
    check sy-subrc = 0.

    data(lv_idx) = lines( ls_thread-messages ).
    while lv_idx > 0.
      read table ls_thread-messages into data(ls_msg) index lv_idx.
      if sy-subrc = 0
         and ls_msg-author = sy-uname
         and ls_msg-text is not initial.
        result = ls_msg-text.
        return.
      endif.
      lv_idx -= 1.
    endwhile.
  endmethod.
  method format_timestamp.
    check iv_timestamp is not initial.
    data lv_date type d.
    data lv_time type t.
    convert time stamp iv_timestamp time zone sy-zonlo
      into date lv_date time lv_time.
    result = |{ lv_date date = user } { lv_time time = user }|.
  endmethod.
  method set_hunk_action.
    data lv_ts type timestampl.
    get time stamp field lv_ts.
    delete mt_hunk_actions where hunk_key = iv_hunk_key and reviewer = sy-uname.
    append value ty_hunk_action(
      hunk_key      = iv_hunk_key
      reviewer      = sy-uname
      reviewer_name = zcl_ave_popup_data=>get_user_name( sy-uname )
      action        = iv_action
      changed_at    = lv_ts ) to mt_hunk_actions.
  endmethod.
  method clear_hunk_action.
    delete mt_hunk_actions where hunk_key = iv_hunk_key and reviewer = sy-uname.
  endmethod.
  method render_hunk_action_meta.
    data ls_action type ty_hunk_action.
    loop at mt_hunk_actions into data(ls_action_cur)
      where hunk_key = iv_hunk_key and action = iv_action.
      if ls_action is initial or ls_action_cur-changed_at > ls_action-changed_at.
        ls_action = ls_action_cur.
      endif.
    endloop.
    check ls_action is not initial.
    data(lv_label) = cond string(
      when iv_action = 'A' then `approved`
      when iv_action = 'D' then `declined`
      else `reviewed` ).
    result =
      | <span style="font-weight:normal;color:#7f8c8d;font-size:10px">| &&
      |{ lv_label } by { escape( val = conv string( ls_action-reviewer ) format = cl_abap_format=>e_html_text ) }| &&
      | / { escape( val = conv string( ls_action-reviewer_name ) format = cl_abap_format=>e_html_text ) }| &&
      | / { escape( val = format_timestamp( ls_action-changed_at ) format = cl_abap_format=>e_html_text ) }</span>|.
  endmethod.
  method get_hunk_global_action.
    data ls_action type ty_hunk_action.
    loop at mt_hunk_actions into data(ls_action_cur)
      where hunk_key = iv_hunk_key.
      if ls_action is initial or ls_action_cur-changed_at > ls_action-changed_at.
        ls_action = ls_action_cur.
      endif.
    endloop.
    result = ls_action-action.
  endmethod.
  method sanitize_review_state.
    check mt_hunk_info is not initial.

    loop at mt_approved into data(lv_approved_key).
      if not line_exists( mt_hunk_info[ hunk_key = lv_approved_key ] ).
        delete table mt_approved from lv_approved_key.
      endif.
    endloop.

    loop at mt_declined into data(lv_declined_key).
      if not line_exists( mt_hunk_info[ hunk_key = lv_declined_key ] ).
        delete table mt_declined from lv_declined_key.
      elseif line_exists( mt_approved[ table_line = lv_declined_key ] ).
        delete table mt_declined from lv_declined_key.
      endif.
    endloop.

    loop at mt_hunk_actions into data(ls_action_key).
      if not line_exists( mt_hunk_info[ hunk_key = ls_action_key-hunk_key ] ).
        delete mt_hunk_actions where hunk_key = ls_action_key-hunk_key.
      endif.
    endloop.
  endmethod.
  method collect_report_status.
    clear: et_approved, et_declined.

    data(ls_payload) = value ty_saved_payload( ).
    if load_review_payload(
         exporting iv_trkorr = conv #( mv_object_name )
         importing es_payload = ls_payload ) = abap_true.
      loop at ls_payload-user_states into data(ls_user_state).
        loop at ls_user_state-approved into data(ls_saved_approved).
          if mt_hunk_info is initial
             or line_exists( mt_hunk_info[ hunk_key = ls_saved_approved-hunk_key ] ).
            insert ls_saved_approved-hunk_key into table et_approved.
          endif.
        endloop.
        loop at ls_user_state-declined into data(ls_saved_declined).
          if mt_hunk_info is initial
             or line_exists( mt_hunk_info[ hunk_key = ls_saved_declined-hunk_key ] ).
            insert ls_saved_declined-hunk_key into table et_declined.
          endif.
        endloop.
      endloop.
    endif.

    loop at mt_approved into data(lv_approved_key).
      insert lv_approved_key into table et_approved.
    endloop.
    loop at mt_declined into data(lv_declined_key).
      insert lv_declined_key into table et_declined.
    endloop.

    if mt_hunk_info is not initial.
      loop at et_approved into lv_approved_key.
        if not line_exists( mt_hunk_info[ hunk_key = lv_approved_key ] ).
          delete table et_approved from lv_approved_key.
        endif.
      endloop.
      loop at et_declined into lv_declined_key.
        if not line_exists( mt_hunk_info[ hunk_key = lv_declined_key ] )
           or line_exists( et_approved[ table_line = lv_declined_key ] ).
          delete table et_declined from lv_declined_key.
        endif.
      endloop.
    endif.
  endmethod.
  method is_own_hunk.
    result = abap_false.
    read table mt_hunk_info into data(ls_hunk)
      with table key hunk_key = iv_hunk_key.
    if sy-subrc = 0 and ls_hunk-author = sy-uname.
      result = abap_true.
    elseif sy-subrc = 0 and ls_hunk-author is initial and ls_hunk-html cs sy-uname.
      result = abap_true.
    endif.
  endmethod.
  method get_reviewer_stats.
    data(ls_payload) = value ty_saved_payload( ).
    data(lv_has_payload) = load_review_payload(
      exporting
        iv_trkorr  = conv #( mv_object_name )
      importing
        es_payload = ls_payload ).

    if lv_has_payload = abap_true.
      loop at ls_payload-user_states into data(ls_user_state).
        data lv_appr_saved type i.
        data lv_decl_saved type i.
        clear: lv_appr_saved, lv_decl_saved.
        loop at ls_user_state-approved into data(ls_saved_appr_key).
          if mt_hunk_info is initial
             or line_exists( mt_hunk_info[ hunk_key = ls_saved_appr_key-hunk_key ] ).
            lv_appr_saved += 1.
          endif.
        endloop.
        loop at ls_user_state-declined into data(ls_saved_decl_key).
          if mt_hunk_info is initial
             or line_exists( mt_hunk_info[ hunk_key = ls_saved_decl_key-hunk_key ] ).
            lv_decl_saved += 1.
          endif.
        endloop.
        check lv_appr_saved > 0 or lv_decl_saved > 0.
        append value zif_ave_acr_types=>ty_reviewer_stats(
          reviewer      = ls_user_state-reviewer
          reviewer_name = ls_user_state-reviewer_name
          appr_count    = lv_appr_saved
          decl_count    = lv_decl_saved
          total_count   = lv_appr_saved + lv_decl_saved
          saved_at      = ls_user_state-saved_at ) to result.
      endloop.
    endif.

    data(lv_appr_cur) = lines( mt_approved ).
    data(lv_decl_cur) = lines( mt_declined ).
    if lv_appr_cur > 0 or lv_decl_cur > 0.
      read table result assigning field-symbol(<rev>)
        with key reviewer = sy-uname.
      if sy-subrc <> 0.
        append value zif_ave_acr_types=>ty_reviewer_stats(
          reviewer      = sy-uname
          reviewer_name = zcl_ave_popup_data=>get_user_name( sy-uname ) ) to result.
        read table result assigning <rev> with key reviewer = sy-uname.
      endif.
      <rev>-appr_count = lv_appr_cur.
      <rev>-decl_count = lv_decl_cur.
      <rev>-total_count = lv_appr_cur + lv_decl_cur.
    endif.

    loop at mt_hunk_threads into data(ls_thread_cur).
      read table mt_hunk_info into data(ls_hunk_cur)
        with table key hunk_key = ls_thread_cur-hunk_key.
      loop at ls_thread_cur-messages into data(ls_msg_cur).
        check ls_msg_cur-author is not initial.
        if sy-subrc = 0 and ls_hunk_cur-author = ls_msg_cur-author.
          continue.
        endif.
        read table result assigning <rev>
          with key reviewer = ls_msg_cur-author.
        if sy-subrc <> 0.
          append value zif_ave_acr_types=>ty_reviewer_stats(
            reviewer      = ls_msg_cur-author
            reviewer_name = ls_msg_cur-author_name ) to result.
          read table result assigning <rev> with key reviewer = ls_msg_cur-author.
        endif.
        if <rev>-total_count = 0.
          <rev>-total_count = 1.
        endif.
      endloop.
    endloop.
  endmethod.
  method save_review_to_db.
    data lv_saved_at type timestampl.
    data lv_tabname type tabname value 'ZAVE_REVIEW'.
    data lv_save_trkorr type trkorr.
    data lr_review_db type ref to data.

    check mv_code_review = abap_true.
    check mv_object_type = zcl_ave_object_factory=>gc_type-tr.
    lv_save_trkorr = conv #( mv_object_name ).
    check lv_save_trkorr is not initial.

    sanitize_review_state( ).

    data(ls_payload) = value ty_saved_payload( ).
    data(lv_has_existing) = load_review_payload(
      exporting
        iv_trkorr  = lv_save_trkorr
      importing
        es_payload = ls_payload ).

    get time stamp field lv_saved_at.
    data(lv_user_name) = zcl_ave_popup_data=>get_user_name( sy-uname ).

    data(ls_user_state_new) = value ty_saved_user_state(
      reviewer      = sy-uname
      reviewer_name = lv_user_name
      saved_at      = lv_saved_at ).

    loop at mt_approved into data(lv_approved_key).
      append value ty_saved_key( hunk_key = lv_approved_key ) to ls_user_state_new-approved.
    endloop.
    loop at mt_declined into data(lv_declined_key).
      append value ty_saved_key( hunk_key = lv_declined_key ) to ls_user_state_new-declined.
    endloop.
    loop at mt_decline_notes into data(ls_note_cur).
      append value ty_saved_note(
        hunk_key = ls_note_cur-hunk_key
        note     = ls_note_cur-note ) to ls_user_state_new-notes.
    endloop.

    ls_payload-schema_version = 1.
    ls_payload-trkorr = lv_save_trkorr.
    ls_payload-last_saved_at = lv_saved_at.
    ls_payload-last_saved_by = sy-uname.
    ls_payload-obj_stats = mt_acr_stats.
    ls_payload-hunks = mt_hunk_info.
    ls_payload-diff_cache = mt_diff_cache.
    ls_payload-hunk_actions = mt_hunk_actions.

    delete ls_payload-user_states where reviewer = sy-uname.
    append ls_user_state_new to ls_payload-user_states.

    loop at mt_hunk_threads into data(ls_thread_cur).
      data(ls_thread_to_save) = value ty_saved_thread(
        hunk_key     = ls_thread_cur-hunk_key
        objtype      = ls_thread_cur-objtype
        obj_name     = ls_thread_cur-obj_name
        class_name   = ls_thread_cur-class_name
        display_name = ls_thread_cur-display_name
        hunk_no      = ls_thread_cur-hunk_no
        start_line   = ls_thread_cur-start_line
        change_count = ls_thread_cur-change_count
        change_kind  = ls_thread_cur-change_kind
        author       = cond #(
          when line_exists( mt_hunk_info[ hunk_key = ls_thread_cur-hunk_key ] )
          then mt_hunk_info[ hunk_key = ls_thread_cur-hunk_key ]-author )
        author_name  = cond #(
          when line_exists( mt_hunk_info[ hunk_key = ls_thread_cur-hunk_key ] )
          then mt_hunk_info[ hunk_key = ls_thread_cur-hunk_key ]-author_name )
        html         = ls_thread_cur-html
        messages     = ls_thread_cur-messages ).

      read table ls_payload-threads assigning field-symbol(<ls_thread_saved>)
        with key hunk_key = ls_thread_cur-hunk_key.
      if sy-subrc <> 0.
        append ls_thread_to_save to ls_payload-threads.
        continue.
      endif.

      <ls_thread_saved>-objtype      = ls_thread_to_save-objtype.
      <ls_thread_saved>-obj_name     = ls_thread_to_save-obj_name.
      <ls_thread_saved>-class_name   = ls_thread_to_save-class_name.
      <ls_thread_saved>-display_name = ls_thread_to_save-display_name.
      <ls_thread_saved>-hunk_no      = ls_thread_to_save-hunk_no.
      <ls_thread_saved>-start_line   = ls_thread_to_save-start_line.
      <ls_thread_saved>-change_count = ls_thread_to_save-change_count.
      <ls_thread_saved>-change_kind  = ls_thread_to_save-change_kind.
      <ls_thread_saved>-author       = ls_thread_to_save-author.
      <ls_thread_saved>-author_name  = ls_thread_to_save-author_name.
      <ls_thread_saved>-html         = ls_thread_to_save-html.

      loop at ls_thread_cur-messages into data(ls_msg_cur).
        read table <ls_thread_saved>-messages transporting no fields
          with key author = ls_msg_cur-author
                   created_at = ls_msg_cur-created_at
                   text = ls_msg_cur-text.
        if sy-subrc <> 0.
          append ls_msg_cur to <ls_thread_saved>-messages.
        endif.
      endloop.
    endloop.

    append value ty_saved_history(
      saved_at       = lv_saved_at
      saved_by       = sy-uname
      saved_by_name  = lv_user_name
      approved_count = lines( mt_approved )
      declined_count = lines( mt_declined )
      note_count     = lines( mt_decline_notes ) ) to ls_payload-history.

    data(lv_payload_json) = /ui2/cl_json=>serialize( data = ls_payload ).
    try.
        update (lv_tabname)
          set payload = @lv_payload_json
          where trkorr = @lv_save_trkorr.
        if sy-subrc <> 0.
          create data lr_review_db type (lv_tabname).
          assign lr_review_db->* to field-symbol(<ls_review_db>).
          if <ls_review_db> is assigned.
            assign component 'TRKORR' of structure <ls_review_db> to field-symbol(<lv_trkorr>).
            assign component 'PAYLOAD' of structure <ls_review_db> to field-symbol(<lv_payload>).
            if <lv_trkorr> is assigned and <lv_payload> is assigned.
              <lv_trkorr> = lv_save_trkorr.
              <lv_payload> = lv_payload_json.
              insert (lv_tabname) from @<ls_review_db>.
            else.
              sy-subrc = 4.
            endif.
          else.
            sy-subrc = 4.
          endif.
        endif.
      catch cx_sy_create_data_error
            cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_syntax
            cx_sy_open_sql_db.
        sy-subrc = 4.
    endtry.

    if iv_silent = abap_true.
      return.
    endif.

    if sy-subrc = 0.
      message |Review saved for { mv_object_name }| type 'S'.
    elseif lv_has_existing = abap_true.
      message |Review for { mv_object_name } could not be updated| type 'E'.
    else.
      message |Review for { mv_object_name } could not be created| type 'E'.
    endif.
  endmethod.
  method build_review_help_html.
    result =
      `<!DOCTYPE html><html><head><meta charset="utf-8"><style>` &&
      `body{font:13px/1.5 Segoe UI,Arial,sans-serif;background:#f7f7f9;color:#222;padding:18px;}` &&
      `h2{margin:0 0 10px;color:#0a6ed1;}p{margin:0 0 12px;}` &&
      `table{border-collapse:collapse;width:100%;background:#fff;margin:10px 0 14px;}` &&
      `th,td{border:1px solid #d9d9d9;padding:7px 9px;text-align:left;vertical-align:top;}` &&
      `th{background:#eef4fb;}code{background:#eef2f7;padding:1px 4px;border-radius:3px;}` &&
      `ol{margin:8px 0 0 22px;padding:0;}li{margin:0 0 6px;}` &&
      `</style></head><body>` &&
      `<h2>Save review requires table ZAVE_REVIEW</h2>` &&
      `<p>The button can save review data only after a transparent table <code>ZAVE_REVIEW</code> is created and activated.</p>` &&
      `<p>For now keep the design minimal: one row per transport request, and the full review with save history stored inside one JSON payload.</p>` &&
      `<table><tr><th>Field</th><th>Type</th><th>Purpose</th></tr>` &&
      `<tr><td>MANDT</td><td>MANDT</td><td>Client field</td></tr>` &&
      `<tr><td>TRKORR</td><td>TRKORR</td><td>Transport request key</td></tr>` &&
      `<tr><td>PAYLOAD</td><td>STRING</td><td>Stored review JSON including current state and save history</td></tr>` &&
      `</table>` &&
      `<ol>` &&
      `<li>Create transparent table <code>ZAVE_REVIEW</code>.</li>` &&
      `<li>Make <code>MANDT</code> and <code>TRKORR</code> key fields.</li>` &&
      `<li>Add field <code>PAYLOAD</code> as type <code>STRING</code>.</li>` &&
      `<li>Activate the table. No ZIP or compression is needed yet.</li>` &&
      `<li>Return to AVE and press <code>Save</code> again.</li>` &&
      `</ol>` &&
      `</body></html>`.
  endmethod.
  method show_review_help_popup.
    if mo_help_box is bound.
      mo_help_box->free( ).
      clear: mo_help_box, mo_help_html.
    endif.

    create object mo_help_box
      exporting
        width                       = 760
        height                      = 360
        top                         = 90
        left                        = 120
        caption                     = 'ZAVE_REVIEW setup'
        lifetime                    = cl_gui_control=>lifetime_dynpro
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
    if sy-subrc <> 0.
      return.
    endif.

    set handler me->on_help_box_close for mo_help_box.

    create object mo_help_html
      exporting
        parent             = mo_help_box
      exceptions
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        others             = 5.
    if sy-subrc <> 0.
      mo_help_box->free( ).
      clear: mo_help_box, mo_help_html.
      return.
    endif.

    data(lv_help_html) = build_review_help_html( ).
    data: lt_html   type w3htmltab,
          lv_url    type w3url,
          lv_offset type i,
          lv_len    type i,
          lv_chunk  type i.

    lv_len = strlen( lv_help_html ).
    while lv_offset < lv_len.
      lv_chunk = cond #( when lv_len - lv_offset > 255 then 255 else lv_len - lv_offset ).
      append value #( line = lv_help_html+lv_offset(lv_chunk) ) to lt_html.
      lv_offset += lv_chunk.
    endwhile.

    mo_help_html->load_data(
      importing
        assigned_url = lv_url
      changing
        data_table   = lt_html
      exceptions
        others       = 1 ).
    if sy-subrc = 0.
      mo_help_html->show_url( url = lv_url ).
      cl_gui_control=>set_focus( control = mo_help_html ).
      cl_gui_cfw=>flush( ).
    endif.
  endmethod.
  method auto_show_diff_or_source.
    data(lt_src) = zcl_ave_popup_data=>get_ver_source(
      i_objtype = is_new-objtype
      i_objname = is_new-objname
      i_versno  = is_new-versno
      i_korrnum = is_new-korrnum
      i_author  = is_new-author
      i_datum   = is_new-datum
      i_zeit    = is_new-zeit ).
    if lines( lt_src ) > 1000.
      show_source( i_objtype = is_new-objtype
                   i_objname = is_new-objname
                   i_versno  = is_new-versno ).
    else.
      show_versions_diff( is_old = is_old is_new = is_new ).
    endif.
  endmethod.
  method show_versions_diff.
    ms_diff_old = is_old.
    ms_diff_new = is_new.
    if mo_box is bound.
      data(lv_new_lbl) = cond string( when is_new-versno_text ca '0123456789' and is_new-versno_text na 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                      then |v{ is_new-versno_text }| else is_new-versno_text ).
      data(lv_old_lbl) = cond string(
        when is_old-versno is initial then `(new object)`
        when is_old-versno_text ca '0123456789' and is_old-versno_text na 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        then |v{ is_old-versno_text }| else is_old-versno_text ).
      data(lv_extra2) = cond string(
        when mv_cur_part_name is not initial
        then | – { mv_cur_part_name }|
        when is_new-objname is not initial and is_new-objname <> mv_object_name
        then | – { is_new-objtype }: { is_new-objname }|
        else `` ).
      mo_box->set_caption( |{ mv_object_type }: { mv_object_name }{ lv_extra2 }  [{ lv_new_lbl } -- { lv_old_lbl }]| ).
    endif.
    " Cache lookup
    data(ls_cache_key) = value ty_diff_cache_key(
      objtype     = is_new-objtype
      objname     = is_new-objname
      versno_o    = is_old-versno
      versno_n    = is_new-versno
      blame       = mv_blame
      two_pane    = mv_two_pane
      compact     = mv_compact
      debug       = mv_debug
      ignore_case = mv_ignore_case ).
    read table mt_diff_cache into data(ls_cached) with table key key = ls_cache_key.
    if sy-subrc = 0.
      set_html( ls_cached-html ).
      return.
    endif.

    try.
        data lt_vrsd_o type vrsd_tab.
        data lt_vrsd_n type vrsd_tab.
        data(lv_vno_o) = zcl_ave_versno=>to_internal( is_old-versno ).
        data(lv_vno_n) = zcl_ave_versno=>to_internal( is_new-versno ).
        select * from vrsd where objtype = @is_old-objtype and objname = @is_old-objname
          and versno = @lv_vno_o into table @lt_vrsd_o up to 1 rows.
        select * from vrsd where objtype = @is_new-objtype and objname = @is_new-objname
          and versno = @lv_vno_n into table @lt_vrsd_n up to 1 rows.
        if lt_vrsd_o is initial.
          append value vrsd( objtype = is_old-objtype objname = is_old-objname
                             versno  = lv_vno_o       korrnum = is_old-korrnum
                             author  = is_old-author   datum   = is_old-datum
                             zeit    = is_old-zeit ) to lt_vrsd_o.
        endif.
        if lt_vrsd_n is initial.
          append value vrsd( objtype = is_new-objtype objname = is_new-objname
                             versno  = lv_vno_n       korrnum = is_new-korrnum
                             author  = is_new-author   datum   = is_new-datum
                             zeit    = is_new-zeit ) to lt_vrsd_n.
        endif.
        " Old source: empty for brand-new objects (no prior version → all-green diff)
        data lt_src_o type abaptxt255_tab.
        if is_old-versno is not initial.
          lt_src_o = new zcl_ave_version( lt_vrsd_o[ 1 ] )->get_source( ).
        endif.
        data(lt_src_n) = new zcl_ave_version( lt_vrsd_n[ 1 ] )->get_source( ).
        data(lt_diff) = zcl_ave_popup_diff=>compute_diff(
          it_old        = lt_src_o
          it_new        = lt_src_n
          i_title       = |{ is_new-objtype }: { is_new-objname }|
          i_confirm_key = |DIFF~{ is_new-objtype }~{ is_new-objname }|
          i_ignore_case = mv_ignore_case ).
        data(lv_meta)  = cond string(
          when is_old-versno is initial then |{ is_new-versno_text } → (new object)|
          else |{ is_new-versno_text } → { is_old-versno_text }| ).
        data lt_blame         type ty_blame_map.
        data lt_blame_deleted type ty_blame_map.
        if mv_blame = abap_true.
          lt_blame = zcl_ave_popup_diff=>build_blame_map(
            exporting
              it_versions      = mt_versions
              i_objtype        = is_new-objtype
              i_objname        = is_new-objname
              i_from           = is_old-versno
              i_to             = is_new-versno
            importing
              et_blame_deleted = lt_blame_deleted ).
        endif.
        data lv_html type string.
        if mv_debug = abap_true.
          lv_html = zcl_ave_popup_html=>debug_diff_html(
            it_diff = lt_diff
            i_title = |{ is_new-objtype }: { is_new-objname }|
            i_meta  = lv_meta ).
        else.
          lv_html = zcl_ave_popup_html=>diff_to_html(
            it_diff          = lt_diff
            i_title          = |{ is_new-objtype }: { is_new-objname }|
            i_meta           = lv_meta
            i_two_pane       = mv_two_pane
            " Force compact for huge files — full view would render millions of rows.
            i_compact        = cond #( when lines( lt_src_o ) > 10000 or lines( lt_src_n ) > 10000
                                       then abap_true else mv_compact )
            i_plain          = cond #( when lines( lt_src_o ) > 10000 or lines( lt_src_n ) > 10000
                                       then abap_true else abap_false )
            i_ignore_case    = mv_ignore_case
            it_blame         = lt_blame
            it_blame_deleted = lt_blame_deleted ).
        endif.
        insert value ty_diff_cache( key = ls_cache_key html = lv_html ) into table mt_diff_cache.
        set_html( lv_html ).
      catch cx_root into data(lx_compare).
        data(lv_err_txt) = escape( val = lx_compare->get_text( ) format = cl_abap_format=>e_html_text ).
        data(lv_err_diffline) = zcl_ave_popup_html=>gv_render_line.
        set_html( |<html><body style="padding:24px;font:13px Consolas;color:#c00">| &&
          |Error loading versions for comparison.<br><br>{ lv_err_txt }| &&
          cond string( when lv_err_diffline > 0
            then |<br><br><span style="color:#888;font-size:11px">diff source line { lv_err_diffline }</span>|
            else `` ) &&
          |</body></html>| ).
    endtry.
  endmethod.
  method cr_precompute_class_parts.
    data(lv_before) = lines( mt_acr_stats ).
    try.
        data(lo_obj) = new zcl_ave_object_factory( )->get_instance(
          object_type = zcl_ave_object_factory=>gc_type-class
          object_name = conv #( i_class_name ) ).
        data(lt_cr_parts) = lo_obj->get_parts( ).
        data(lv_cr_total) = lines( lt_cr_parts ).
        loop at lt_cr_parts into data(ls_part).
          call function 'SAPGUI_PROGRESS_INDICATOR'
            exporting
              percentage = conv i( sy-tabix * 100 / cond i( when lv_cr_total > 0 then lv_cr_total else 1 ) )
              text       = conv char70( |Code Review: precomputing part { sy-tabix }/{ lv_cr_total }| ).
          check ls_part-type <> 'CLSD' and ls_part-type <> 'RELE'.
          cr_precompute_part( value #(
            type        = ls_part-type
            name        = ls_part-unit
            class       = ls_part-class
            object_name = ls_part-object_name ) ).
        endloop.
      catch cx_root.
    endtry.
    result = boolc( lines( mt_acr_stats ) > lv_before ).
  endmethod.
  method cr_precompute_part.
    " CLAS rows are aggregate markers — they have no direct diff source
    check is_part-type <> 'CLAS'.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 0
        text       = conv char70( |Code Review: loading versions for { is_part-object_name }| ).

    " Use load_versions — same as Version Explorer — fills mt_versions with
    " correct obj_owner (nearest-task logic), trfunction, datum, zeit.
    load_versions( i_objtype = is_part-type i_objname = is_part-object_name ).
    check mt_versions is not initial.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 20
        text       = conv char70( |Code Review: locating TR version for { is_part-object_name }| ).

    " Build range: request + all its tasks
    data lt_korr_range type range of verskorrno.
    data(lv_req) = conv verskorrno( mv_object_name ).
    append value #( sign = 'I' option = 'EQ' low = lv_req ) to lt_korr_range.
    select trkorr from e070 where strkorr = @lv_req into table @data(lt_tasks_cr).
    loop at lt_tasks_cr into data(ls_task_cr).
      append value #( sign = 'I' option = 'EQ' low = conv verskorrno( ls_task_cr-trkorr ) )
        to lt_korr_range.
    endloop.

    " Find new version (belongs to this transport) and prior version — same as user does in VE
    data ls_new type ty_version_row.
    data ls_old type ty_version_row.
    data lv_idx type i.
    loop at mt_versions into ls_new where korrnum = lv_req.
      lv_idx = sy-tabix.
      exit.
    endloop.
    if ls_new is initial.
      loop at mt_versions into ls_new.
        if ls_new-korrnum in lt_korr_range.
          lv_idx = sy-tabix.
          exit.
        endif.
      endloop.
    endif.
    check ls_new is not initial.

    clear ls_old.
    loop at mt_versions into ls_old from lv_idx + 1 where trfunction = 'K'.
      exit.
    endloop.
    data(lv_is_created) = cond abap_bool( when ls_old is initial then abap_true else abap_false ).
    data lv_missing_initial_history type abap_bool.
    data lv_tadir_author type tadir-author.

    if lv_is_created = abap_true.
      data lv_tadir_type type tadir-object.
      data lv_tadir_name type tadir-obj_name.
      lv_tadir_type = switch tadir-object( is_part-type
        when 'REPS' or 'REPT' then 'PROG'
        when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' or 'METH' then 'CLAS'
        else is_part-type ).
      lv_tadir_name = cond #( when lv_tadir_type = 'CLAS' and is_part-class is not initial
                              then conv tadir-obj_name( is_part-class )
                              else conv tadir-obj_name( is_part-object_name ) ).
      if lv_tadir_type = 'CLAS' and lv_tadir_name cs '='.
        data(lv_cls_eq_pos) = find( val = conv string( lv_tadir_name ) sub = '=' ).
        if lv_cls_eq_pos > 0.
          lv_tadir_name = lv_tadir_name(lv_cls_eq_pos).
        endif.
      endif.

      data lv_tadir_created_on type tadir-created_on.
      select single author, created_on
        from tadir
        where pgmid    = 'R3TR'
          and object   = @lv_tadir_type
          and obj_name = @lv_tadir_name
          and delflag  = ' '
        into (@lv_tadir_author, @lv_tadir_created_on).

      data ls_first_available type ty_version_row.
      loop at mt_versions into data(ls_first_scan).
        if ls_first_available is initial or ls_first_scan-versno < ls_first_available-versno.
          ls_first_available = ls_first_scan.
        endif.
      endloop.

      " No prior K means the review treats the object as new. Do not replace
      " it with an older non-K baseline based on TADIR creation metadata.
    endif.

    if mv_filter_user is not initial.
      data(lv_effective_author) = cond versuser(
        when lv_missing_initial_history = abap_true and lv_tadir_author is not initial then lv_tadir_author
        when ls_new-obj_owner is not initial then ls_new-obj_owner
        else ls_new-author ).
      if lv_effective_author <> mv_filter_user.
        return.
      endif.
    endif.

    data(lv_versno_new) = ls_new-versno.
    data(lv_versno_old) = ls_old-versno.

    try.
        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = 30
            text       = conv char70( |Code Review: loading new source for { is_part-object_name }| ).
        " Load sources — same as show_versions_diff
        data lt_vrsd_n type vrsd_tab.
        data(lv_vno_n) = zcl_ave_versno=>to_internal( lv_versno_new ).
        select * from vrsd where objtype = @is_part-type and objname = @is_part-object_name
          and versno = @lv_vno_n into table @lt_vrsd_n up to 1 rows.
        if lt_vrsd_n is initial.
          append value vrsd( objtype = is_part-type objname = is_part-object_name
                             versno = lv_vno_n ) to lt_vrsd_n.
        endif.
        data(lt_src_n) = new zcl_ave_version( lt_vrsd_n[ 1 ] )->get_source( ).
        " Old source: empty for brand-new objects (no prior version → all-green diff)
        data lt_src_o type abaptxt255_tab.
        if ls_old is not initial.
          call function 'SAPGUI_PROGRESS_INDICATOR'
            exporting
              percentage = 40
              text       = conv char70( |Code Review: loading old source for { is_part-object_name }| ).

          data lt_vrsd_o type vrsd_tab.
          data(lv_vno_o) = zcl_ave_versno=>to_internal( lv_versno_old ).
          select * from vrsd where objtype = @is_part-type and objname = @is_part-object_name
            and versno = @lv_vno_o into table @lt_vrsd_o up to 1 rows.
          if lt_vrsd_o is initial.
            append value vrsd( objtype = is_part-type objname = is_part-object_name
                               versno = lv_vno_o ) to lt_vrsd_o.
          endif.
          lt_src_o = new zcl_ave_version( lt_vrsd_o[ 1 ] )->get_source( ).
        elseif lv_missing_initial_history = abap_true.
          lt_src_o = lt_src_n.
        endif.

        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = 50
            text       = conv char70( |Code Review: computing diff for { is_part-object_name }| ).

        data(lt_diff) = zcl_ave_popup_diff=>compute_diff(
          it_old        = lt_src_o
          it_new        = lt_src_n
          i_title       = conv #( is_part-object_name )
          i_confirm_key = |DIFF~{ is_part-type }~{ is_part-object_name }|
          i_ignore_case = mv_ignore_case ).

        " Blame — pass mt_versions directly, same as show_versions_diff
        data lt_blame         type ty_blame_map.
        data lt_blame_deleted type ty_blame_map.
        if mv_blame = abap_true and ls_old is not initial and lines( lt_src_o ) <= 1000 and lines( lt_src_n ) <= 1000.
          call function 'SAPGUI_PROGRESS_INDICATOR'
            exporting
              percentage = 65
              text       = conv char70( |Code Review: computing blame for { is_part-object_name }| ).

          lt_blame = zcl_ave_popup_diff=>build_blame_map(
            exporting
              it_versions      = mt_versions
              i_objtype        = is_part-type
              i_objname        = is_part-object_name
              i_from           = lv_versno_old
              i_to             = lv_versno_new
            importing
              et_blame_deleted = lt_blame_deleted ).
        elseif mv_blame = abap_true.
          call function 'SAPGUI_PROGRESS_INDICATOR'
            exporting
              percentage = 65
              text       = conv char70( |Code Review: skipping blame for large source { is_part-object_name }| ).
        endif.

        " Render HTML — same as show_versions_diff
        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = 75
            text       = conv char70( |Code Review: rendering diff for { is_part-object_name }| ).

        data(lv_meta_cr) = cond string(
          when lv_is_created = abap_true
          then |{ ls_new-versno_text } → (new object)|
          else |{ ls_new-versno_text } → { ls_old-versno_text }| ).
        if lv_missing_initial_history = abap_true.
          lv_meta_cr = |{ ls_new-versno_text } -> (missing earlier versions)|.
        endif.
        data(lv_html) = zcl_ave_popup_html=>diff_to_html(
          it_diff          = lt_diff
          i_title          = |{ is_part-type }: { is_part-object_name }|
          i_meta           = lv_meta_cr
          i_two_pane       = mv_two_pane
          i_compact        = cond #( when lines( lt_src_o ) > 10000 or lines( lt_src_n ) > 10000
                                     then abap_true else mv_compact )
          i_plain          = cond #( when lines( lt_src_o ) > 10000 or lines( lt_src_n ) > 10000
                                     then abap_true else abap_false )
          i_ignore_case    = mv_ignore_case
          i_code_review    = abap_true
          it_blame         = lt_blame
          it_blame_deleted = lt_blame_deleted ).

        call function 'SAPGUI_PROGRESS_INDICATOR'
          exporting
            percentage = 85
            text       = conv char70( |Code Review: collecting hunks for { is_part-object_name }| ).

        data lt_hunk_html type string_table.
        data lv_rows_html type string.
        data lv_tb_off type i.
        data lv_tb_len type i.
        find first occurrence of `<table><tbody>` in lv_html
          match offset lv_tb_off match length lv_tb_len.
        if sy-subrc = 0.
          data(lv_rows_start) = lv_tb_off + lv_tb_len.
          data(lv_rows_tail) = lv_html+lv_rows_start.
          data lv_rows_end type i.
          find first occurrence of `</tbody></table>` in lv_rows_tail match offset lv_rows_end.
          if sy-subrc = 0.
            lv_rows_html = lv_rows_tail(lv_rows_end).
          endif.
        endif.
        if lv_rows_html is not initial.
          data lv_scan_off type i value 0.
          do.
            data(lv_scan_tail) = lv_rows_html+lv_scan_off.
            data lv_add_rel type i.
            data lv_del_rel type i.
            data lv_has_add type abap_bool.
            data lv_has_del type abap_bool.
            clear: lv_add_rel, lv_del_rel, lv_has_add, lv_has_del.
            find first occurrence of `<tr style="background:#e8f4e8` in lv_scan_tail match offset lv_add_rel.
            if sy-subrc = 0. lv_has_add = abap_true. endif.
            find first occurrence of `<tr style="background:#fdf0f0` in lv_scan_tail match offset lv_del_rel.
            if sy-subrc = 0. lv_has_del = abap_true. endif.
            if lv_has_add = abap_false and lv_has_del = abap_false.
              exit.
            endif.
            data(lv_hstart_rel) = cond i(
              when lv_has_add = abap_true and lv_has_del = abap_true and lv_add_rel <= lv_del_rel then lv_add_rel
              when lv_has_add = abap_true and lv_has_del = abap_false then lv_add_rel
              else lv_del_rel ).
            data(lv_hstart) = lv_scan_off + lv_hstart_rel.
            data(lv_next_start) = lv_hstart + 1.
            data(lv_next_tail) = lv_rows_html+lv_next_start.
            clear: lv_add_rel, lv_del_rel, lv_has_add, lv_has_del.
            find first occurrence of `<tr style="background:#e8f4e8` in lv_next_tail match offset lv_add_rel.
            if sy-subrc = 0. lv_has_add = abap_true. endif.
            find first occurrence of `<tr style="background:#fdf0f0` in lv_next_tail match offset lv_del_rel.
            if sy-subrc = 0. lv_has_del = abap_true. endif.
            data(lv_hend) = strlen( lv_rows_html ).
            if lv_has_add = abap_true or lv_has_del = abap_true.
              data(lv_next_rel) = cond i(
                when lv_has_add = abap_true and lv_has_del = abap_true and lv_add_rel <= lv_del_rel then lv_add_rel
                when lv_has_add = abap_true and lv_has_del = abap_false then lv_add_rel
                else lv_del_rel ).
              lv_hend = lv_hstart + 1 + lv_next_rel.
            endif.
            data(lv_ctx_start) = lv_hstart.
            if mv_compact = abap_false.
              lv_ctx_start = 0.
            else.
              do 3 times.
                data(lv_before_rows) = lv_rows_html(lv_ctx_start).
                data(lv_rev_rows) = reverse( lv_before_rows ).
                find first occurrence of `rt<` in lv_rev_rows match offset data(lv_prev_tr_rev).
                if sy-subrc <> 0.
                  exit.
                endif.
                lv_ctx_start = strlen( lv_before_rows ) - lv_prev_tr_rev - 3.
                if lv_ctx_start <= 0.
                  lv_ctx_start = 0.
                  exit.
                endif.
              enddo.
            endif.
            data(lv_hlen) = lv_hend - lv_ctx_start.
            append lv_rows_html+lv_ctx_start(lv_hlen) to lt_hunk_html.
            lv_scan_off = lv_hend.
          enddo.
        endif.

        insert value ty_diff_cache(
          key  = value #(
            objtype     = is_part-type
            objname     = is_part-object_name
            versno_o    = lv_versno_old
            versno_n    = lv_versno_new
            blame       = mv_blame
            two_pane    = mv_two_pane
            compact     = mv_compact
            debug       = mv_debug
            ignore_case = mv_ignore_case )
          html = lv_html )
          into table mt_diff_cache.

        " Compute ins/del/mod statistics
        data lv_ins type i. data lv_del type i. data lv_mod type i.
        data lt_auth type zif_ave_acr_types=>ty_t_author_stats.
        zcl_ave_acr_stats=>from_diff(
          exporting
            it_diff    = lt_diff
            it_blame   = lt_blame
          importing
            ev_ins     = lv_ins
            ev_del     = lv_del
            ev_mod     = lv_mod
            et_authors = lt_auth ).
        " Owner and date/time — taken from ls_new (already enriched by load_versions).
        " Brand-new objects belong to the creator: owner of the first version.
        data(lv_author) = cond versuser(
          when lv_missing_initial_history = abap_true
               and lv_tadir_author is not initial
               and ls_new-versno = ls_first_available-versno
          then lv_tadir_author
          when lv_is_created = abap_true and mv_cur_creator is not initial
          then mv_cur_creator
          when lv_is_created = abap_true and mt_versions is not initial and mt_versions[ lines( mt_versions ) ]-obj_owner is not initial
          then mt_versions[ lines( mt_versions ) ]-obj_owner
          when lv_is_created = abap_true and mt_versions is not initial
          then mt_versions[ lines( mt_versions ) ]-author
          when ls_new-obj_owner is not initial then ls_new-obj_owner
          else ls_new-author ).
        data(lv_datum)  = ls_new-datum.
        data(lv_zeit)   = ls_new-zeit.

        " Display name: method name / section label for class parts
        data(lv_disp_name) = conv string( is_part-name ).

        " Count change blocks (hunks) from diff, skipping whitespace-only hunks
        data lv_hunk_cnt  type i value 0.
        data lv_in_hunk   type abap_bool value abap_false.
        data lt_cur_hunk  type string_table.
        data lv_new_line   type i value 0.
        data lv_hunk_line  type i.
        data lv_hunk_chg   type i.
        data lv_hunk_ins   type i.
        data lv_hunk_del   type i.
        data lv_hunk_kind  type string.
        data lv_hunk_auth  type versuser.
        delete mt_hunk_info where objtype = is_part-type and obj_name = is_part-object_name.
        loop at lt_diff into data(ls_dop).
          case ls_dop-op.
            when '+' or '-'.
              if lv_in_hunk = abap_false.
                lv_in_hunk = abap_true.
                clear: lt_cur_hunk, lv_hunk_chg, lv_hunk_ins, lv_hunk_del, lv_hunk_auth.
                lv_hunk_line = lv_new_line + 1.
              endif.
              lv_hunk_chg += 1.
              if ls_dop-op = '+'.
                lv_hunk_ins += 1.
              elseif ls_dop-op = '-'.
                lv_hunk_del += 1.
              endif.
              append conv string( ls_dop-text ) to lt_cur_hunk.
              if ls_dop-op = '+'.
                if lv_hunk_auth is initial and lt_blame is not initial.
                  read table lt_blame into data(ls_hb) with key text = ls_dop-text.
                  if sy-subrc = 0. lv_hunk_auth = ls_hb-author. endif.
                endif.
                lv_new_line += 1.
              endif.
            when others.
              if lv_in_hunk = abap_true.
                if zcl_ave_acr_stats=>is_blank_hunk( lt_cur_hunk ) = abap_false.
                  lv_hunk_cnt += 1.
                  lv_hunk_kind = cond string(
                    when lv_hunk_ins > 0 and lv_hunk_del > 0 then `changed`
                    when lv_hunk_ins > 0 then `added`
                    when lv_hunk_del > 0 then `deleted`
                    else `changed` ).
                  data(lv_hunk_key) = |{ is_part-type }~{ is_part-object_name }~{ lv_hunk_cnt }|.
                  data(lv_info_author) = cond versuser(
                    when lv_is_created = abap_true then lv_author
                    when lv_hunk_auth is not initial then lv_hunk_auth
                    else lv_author ).
                  data lv_info_html type string.
                  read table lt_hunk_html into lv_info_html index lv_hunk_cnt.
                  insert value ty_hunk_info(
                    hunk_key     = lv_hunk_key
                    objtype      = is_part-type
                    obj_name     = is_part-object_name
                    class_name   = conv #( is_part-class )
                    display_name = lv_disp_name
                    hunk_no      = lv_hunk_cnt
                    start_line   = lv_hunk_line
                    change_count = lv_hunk_chg
                    change_kind  = lv_hunk_kind
                    author       = lv_info_author
                    author_name  = zcl_ave_popup_data=>get_user_name( lv_info_author )
                    html         = lv_info_html )
                    into table mt_hunk_info.
                endif.
                lv_in_hunk = abap_false.
                clear: lt_cur_hunk, lv_hunk_chg, lv_hunk_ins, lv_hunk_del, lv_hunk_auth.
              endif.
              lv_new_line += 1.
          endcase.
        endloop.
        " flush last hunk if diff ends without '='
        if lv_in_hunk = abap_true and zcl_ave_acr_stats=>is_blank_hunk( lt_cur_hunk ) = abap_false.
          lv_hunk_cnt += 1.
          lv_hunk_kind = cond string(
            when lv_hunk_ins > 0 and lv_hunk_del > 0 then `changed`
            when lv_hunk_ins > 0 then `added`
            when lv_hunk_del > 0 then `deleted`
            else `changed` ).
          data(lv_last_hunk_key) = |{ is_part-type }~{ is_part-object_name }~{ lv_hunk_cnt }|.
          data(lv_last_info_author) = cond versuser(
            when lv_is_created = abap_true then lv_author
            when lv_hunk_auth is not initial then lv_hunk_auth
            else lv_author ).
          data lv_last_info_html type string.
          read table lt_hunk_html into lv_last_info_html index lv_hunk_cnt.
          insert value ty_hunk_info(
            hunk_key     = lv_last_hunk_key
            objtype      = is_part-type
            obj_name     = is_part-object_name
            class_name   = conv #( is_part-class )
            display_name = lv_disp_name
            hunk_no      = lv_hunk_cnt
            start_line   = lv_hunk_line
            change_count = lv_hunk_chg
            change_kind  = lv_hunk_kind
            author       = lv_last_info_author
            author_name  = zcl_ave_popup_data=>get_user_name( lv_last_info_author )
            html         = lv_last_info_html )
            into table mt_hunk_info.
        endif.

        if lv_is_created = abap_true.
          clear lt_auth.
          append value zif_ave_acr_types=>ty_author_stats(
            author      = lv_author
            author_name = zcl_ave_popup_data=>get_user_name( lv_author )
            ins_count   = lv_ins
            del_count   = lv_del
            mod_count   = lv_mod
            hunk_count  = lv_hunk_cnt ) to lt_auth.
        endif.

        " Keep report owner block totals aligned with the user drilldown,
        " which is rendered from mt_hunk_info.
        loop at lt_auth assigning field-symbol(<auth_cnt>).
          clear <auth_cnt>-hunk_count.
        endloop.
        loop at mt_hunk_info into data(ls_auth_hi)
          where objtype = is_part-type and obj_name = is_part-object_name.
          check ls_auth_hi-author is not initial.
          read table lt_auth assigning <auth_cnt> with key author = ls_auth_hi-author.
          if sy-subrc <> 0.
            append value zif_ave_acr_types=>ty_author_stats(
              author      = ls_auth_hi-author
              author_name = ls_auth_hi-author_name ) to lt_auth.
            read table lt_auth assigning <auth_cnt> with key author = ls_auth_hi-author.
          endif.
          <auth_cnt>-hunk_count += 1.
        endloop.

        append value zif_ave_acr_types=>ty_obj_stats(
          objtype      = is_part-type
          class_name   = conv #( is_part-class )
          obj_name     = is_part-object_name
          display_name = lv_disp_name
          versno_new   = lv_versno_new
          versno_old   = lv_versno_old
          author       = lv_author
          author_name  = zcl_ave_popup_data=>get_user_name( lv_author )
          datum        = lv_datum
          zeit         = lv_zeit
          ins_count    = lv_ins
          del_count    = lv_del
          mod_count    = lv_mod
          hunk_count   = lv_hunk_cnt
          bt_authors   = lt_auth
          is_created   = lv_is_created )
          to mt_acr_stats.

      catch cx_root.
        " Skip this part on any error — report will simply omit it
    endtry.
  endmethod.
  method inject_approve_btn.
    result = iv_html.

    " ── Blame info rows end with ' ──</td>' (unique to blame separators) ──
    constants lc_blame type string value ` ──</td>`.
    data lt_bm type match_result_tab.
    find all occurrences of lc_blame in result results lt_bm.

    data lv_total_hunks type i.
    data lv_expected_hunks type i.
    data lv_key_tld type i.
    find first occurrence of '~' in iv_key match offset lv_key_tld.
    if sy-subrc = 0.
      data lv_key_type type versobjtyp.
      data lv_key_name type versobjnam.
      lv_key_type = iv_key(lv_key_tld).
      data(lv_key_name_start) = lv_key_tld + 1.
      lv_key_name = iv_key+lv_key_name_start.
      loop at mt_hunk_info transporting no fields
        where objtype = lv_key_type and obj_name = lv_key_name.
        lv_expected_hunks += 1.
      endloop.
    endif.

    if lt_bm is not initial.
      " Replace from end → start so earlier offsets stay valid
      data(lv_total) = lines( lt_bm ).
      data(lv_pair_markers) = xsdbool(
        mv_two_pane = abap_true
        and lv_expected_hunks > 0
        and lv_total = lv_expected_hunks * 2 ).
      lv_total_hunks = cond i(
        when lv_pair_markers = abap_true then lv_expected_hunks
        else lv_total ).
      sort lt_bm by offset descending.
      loop at lt_bm into data(ls_bm).
        data(lv_marker_no) = lv_total - sy-tabix + 1.
        if lv_pair_markers = abap_true and lv_marker_no mod 2 = 1.
          continue.
        endif.
        data(lv_n) = cond i(
          when lv_pair_markers = abap_true then lv_marker_no / 2
          else lv_marker_no ).   " 1 = topmost blame row
        data(lv_ck) = |{ iv_key }~{ lv_n }|.
        data lv_ins type string.
        data(lv_note_html) = render_decline_thread_html( lv_ck ).
        data(lv_own_ck) = is_own_hunk( lv_ck ).
        data(lv_global_ck) = get_hunk_global_action( lv_ck ).
        if lv_own_ck = abap_true
           and not line_exists( mt_approved[ table_line = lv_ck ] )
           and not line_exists( mt_declined[ table_line = lv_ck ] )
           and lv_global_ck is initial.
          lv_ins = |<a id="acr_c{ lv_n }"></a> --| &&
                   `<span style="margin-left:10px;color:#7f8c8d;` &&
                   `font-style:normal;font-size:12px;font-weight:bold">&#9675; own block</span>` &&
                   render_comment_links( lv_ck ) && `</td>` &&
                   lv_note_html.
        else.
          if line_exists( mt_approved[ table_line = lv_ck ] ).
            lv_ins = |<a id="acr_c{ lv_n }"></a> --| &&
                     `<span style="margin-left:10px;color:#27ae60;` &&
                     `font-style:normal;font-size:12px;font-weight:bold">&#10003; approved</span>` &&
                     render_hunk_action_meta( iv_hunk_key = lv_ck iv_action = 'A' ) &&
                     |<a href="sapevent:undo~{ lv_ck }"| &&
                     ` style="margin-left:8px;background:#95a5a6;color:#fff;font-weight:bold;` &&
                     `text-decoration:none;font-style:normal;font-size:11px;` &&
                     `border-radius:3px;padding:2px 7px">Undo</a>` &&
                     render_comment_links( lv_ck ) && `</td>` &&
                     lv_note_html.
          elseif line_exists( mt_declined[ table_line = lv_ck ] ).
            lv_ins = |<a id="acr_c{ lv_n }"></a> --| &&
                     `<span style="margin-left:10px;color:#e74c3c;` &&
                     `font-style:normal;font-size:12px;font-weight:bold">&#10007; declined</span>` &&
                     render_hunk_action_meta( iv_hunk_key = lv_ck iv_action = 'D' ) &&
                     |<a href="sapevent:undo~{ lv_ck }"| &&
                     ` style="margin-left:8px;background:#95a5a6;color:#fff;font-weight:bold;` &&
                     `text-decoration:none;font-style:normal;font-size:11px;` &&
                     `border-radius:3px;padding:2px 7px">Undo</a>` &&
                     render_comment_links( lv_ck ) && `</td>` &&
                     lv_note_html.
          elseif lv_global_ck = 'A' or lv_global_ck = 'D'.
            lv_ins = |<a id="acr_c{ lv_n }"></a> --|.
            if lv_global_ck = 'A'.
              lv_ins = lv_ins &&
                       `<span style="margin-left:10px;color:#27ae60;` &&
                       `font-style:normal;font-size:12px;font-weight:bold">&#10003; approved</span>` &&
                       render_hunk_action_meta( iv_hunk_key = lv_ck iv_action = 'A' ).
            else.
              lv_ins = lv_ins &&
                       `<span style="margin-left:10px;color:#e74c3c;` &&
                       `font-style:normal;font-size:12px;font-weight:bold">&#10007; declined</span>` &&
                       render_hunk_action_meta( iv_hunk_key = lv_ck iv_action = 'D' ).
            endif.
            if lv_own_ck = abap_false.
              lv_ins = lv_ins &&
                       |<a href="sapevent:approve~{ lv_ck }"| &&
                       ` style="margin-left:10px;background:#27ae60;color:#fff;` &&
                       `text-decoration:none;font-style:normal;font-size:11px;font-weight:bold;` &&
                       `border-radius:3px;padding:2px 7px">&#10003; approve</a>` &&
                       |<a href="sapevent:decline~{ lv_ck }"| &&
                       ` style="margin-left:8px;background:#922b21;color:#fff;` &&
                       `text-decoration:none;font-style:normal;font-size:11px;font-weight:bold;` &&
                       `border-radius:3px;padding:2px 7px">&#10007; decline</a>`.
            endif.
            lv_ins = lv_ins && render_comment_links( lv_ck ) && `</td>` && lv_note_html.
          else.
            lv_ins = |<a id="acr_c{ lv_n }"></a> ──| &&
                     |<a href="sapevent:approve~{ lv_ck }"| &&
                     ` style="margin-left:10px;background:#27ae60;color:#fff;` &&
                     `text-decoration:none;font-style:normal;font-size:11px;font-weight:bold;` &&
                     `border-radius:3px;padding:2px 7px">&#10003; approve</a>` &&
                     |<a href="sapevent:decline~{ lv_ck }"| &&
                     ` style="margin-left:8px;background:#922b21;color:#fff;` &&
                     `text-decoration:none;font-style:normal;font-size:11px;font-weight:bold;` &&
                     `border-radius:3px;padding:2px 7px">&#10007; decline</a>` &&
                     render_comment_links( lv_ck ) && `</td>` &&
                     lv_note_html.
          endif.
        endif.
        data lv_off   type i.
        data lv_after type i.
        lv_off   = ls_bm-offset.
        lv_after = ls_bm-offset + ls_bm-length.
        result = result(lv_off) && lv_ins && result+lv_after.
      endloop.

    else.
      " ── Fallback: compact '...' separator rows ──
      constants lc_sep1 type string value
        `<tr style="background:#f0f0f0;color:#888"><td class="ln">...</td><td class="cd">...</td></tr>`.
      constants lc_sep2 type string value
        `<tr style="background:#f0f0f0;color:#888"><td class="ln">...</td><td class="cd">...</td><td class="sep"></td><td class="ln">...</td><td class="cd">...</td></tr>`.
      data lv_sn type i value 0.
      data lv_found type abap_bool.
      do.
        lv_found = abap_false.
        if result cs lc_sep2.
          lv_found = abap_true.
          lv_sn += 1.
          data(lv_cell2) = me->acr_approve_cell( iv_key = |{ iv_key }~{ lv_sn }| ).
          replace first occurrence of lc_sep2 in result with
            `<tr style="background:#f0f0f0;color:#888"><td class="ln">...</td>` &&
            `<td class="cd">...</td><td class="sep"></td><td class="ln">...</td>` &&
            lv_cell2 && `</tr>`.
        elseif result cs lc_sep1.
          lv_found = abap_true.
          lv_sn += 1.
          data(lv_cell1) = me->acr_approve_cell( iv_key = |{ iv_key }~{ lv_sn }| ).
          replace first occurrence of lc_sep1 in result with
            `<tr style="background:#f0f0f0;color:#888"><td class="ln">...</td>` &&
            lv_cell1 && `</tr>`.
        endif.
        if lv_found = abap_false. exit. endif.
      enddo.
      lv_total_hunks = lv_sn.

      " Single hunk, no separator — fixed button
      if lv_sn = 0.
        lv_total_hunks = 1.
        result = replace( val  = result
                          sub  = `</body>`
                          with = me->acr_approve_fixed( iv_key = |{ iv_key }~1| ) && `</body>` ).
      endif.
    endif.

    " ── Store hunk count in stats ────────────────────────────────────
    data lv_tld type i.
    find first occurrence of '~' in iv_key match offset lv_tld.
    if sy-subrc = 0.
      data lv_type  type versobjtyp.
      data lv_oname type versobjnam.
      lv_type = iv_key(lv_tld).
      data lv_nstart type i.
      lv_nstart = lv_tld + 1.
      lv_oname = iv_key+lv_nstart.
      read table mt_acr_stats assigning field-symbol(<acrs>)
        with key objtype = lv_type obj_name = lv_oname.
      if sy-subrc = 0 and lv_total_hunks > <acrs>-hunk_count.
        <acrs>-hunk_count = lv_total_hunks.
      endif.
    endif.

    " ── "Approve All changes" fixed button (top-right) ──────────────
    data lv_appr_cnt type i value 0.
    data lv_decl_cnt type i value 0.
    do lv_total_hunks times.
      data(lv_count_key) = |{ iv_key }~{ sy-index }|.
      data(lv_count_global) = get_hunk_global_action( lv_count_key ).
      if line_exists( mt_approved[ table_line = lv_count_key ] )
         or lv_count_global = 'A'.
        lv_appr_cnt += 1.
      elseif line_exists( mt_declined[ table_line = lv_count_key ] )
          or lv_count_global = 'D'.
        lv_decl_cnt += 1.
      endif.
    enddo.

    " Badge: ✓N (green) / ✗M (red) / total — always visible
    data(lv_badge) =
      |<span style="color:#27ae60">&#10003;{ lv_appr_cnt }</span>| &&
      | <span style="color:#e74c3c">&#10007;{ lv_decl_cnt }</span>| &&
      | <span style="color:#ccc">/{ lv_total_hunks }</span>|.

    data lv_all_btn type string.
    if lv_appr_cnt >= lv_total_hunks and lv_total_hunks > 0.
      " All approved — static green label
      lv_all_btn =
        `<div style="position:fixed;top:8px;right:12px;z-index:999;` &&
        `background:#27ae60;color:#fff;padding:5px 16px;border-radius:4px;` &&
        `font:bold 12px Consolas,sans-serif">` &&
        |&#10003; All Approved &nbsp;{ lv_badge }</div>|.
    else.
      " Clickable blue button
      lv_all_btn =
        |<div style="position:fixed;top:8px;right:12px;z-index:999">| &&
        |<a href="sapevent:approveall~{ iv_key }"| &&
        ` style="background:#2F2F2F;color:#fff;padding:5px 16px;` &&
        `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">` &&
        |&#10003; Approve All &nbsp;{ lv_badge }</a></div>|.
    endif.
    result = replace( val = result sub = `</body>` with = lv_all_btn && `</body>` ).

    " ── Back button (top-left) ───────────────────────────────────────
    data(lv_back_btn) =
      `<div style="position:fixed;top:8px;left:8px;z-index:999">` &&
      `<a href="sapevent:back~0"` &&
      ` style="background:#3498db;color:#fff;padding:5px 14px;` &&
      `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">` &&
      `&#8592; Back</a></div>`.
    result = replace( val = result sub = `</body>` with = lv_back_btn && `</body>` ).

  endmethod.
  method acr_approve_cell.
    " Returns <td class="cd"> content for a separator row (inline approve/decline links)
    data(lv_own_hunk) = is_own_hunk( iv_key ).
    data(lv_global_action) = get_hunk_global_action( iv_key ).
    if line_exists( mt_approved[ table_line = iv_key ] ).
      result = `<td class="cd" style="color:#27ae60;font-weight:bold">` &&
               `&#10003;&nbsp;approved` &&
               render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'A' ).
      if lv_own_hunk = abap_false.
        result = result &&
                 |<a href="sapevent:undo~{ iv_key }"| &&
                 ` style="margin-left:8px;background:#95a5a6;color:#fff;font-weight:bold;` &&
                 `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">Undo</a>`.
      endif.
      result = result && render_comment_links( iv_key ) && `</td>`.
    elseif line_exists( mt_declined[ table_line = iv_key ] ).
      result = `<td class="cd" style="color:#e74c3c;font-weight:bold">` &&
               `&#10007;&nbsp;declined` &&
               render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'D' ).
      if lv_own_hunk = abap_false.
        result = result &&
                 |<a href="sapevent:undo~{ iv_key }"| &&
                 ` style="margin-left:8px;background:#95a5a6;color:#fff;font-weight:bold;` &&
                 `text-decoration:none;font-size:11px;border-radius:3px;padding:2px 7px">Undo</a>`.
      endif.
      result = result && render_comment_links( iv_key ) && `</td>`.
    elseif lv_global_action = 'A' or lv_global_action = 'D'.
      if lv_global_action = 'A'.
        result = `<td class="cd" style="color:#27ae60;font-weight:bold">` &&
                 `&#10003;&nbsp;approved` &&
                 render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'A' ).
      else.
        result = `<td class="cd" style="color:#e74c3c;font-weight:bold">` &&
                 `&#10007;&nbsp;declined` &&
                 render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'D' ).
      endif.
      if lv_own_hunk = abap_false.
        result = result &&
                 |<a href="sapevent:approve~{ iv_key }"| &&
                 | style="margin-left:12px;background:#27ae60;color:#fff;| &&
                 |font-size:11px;font-weight:bold;text-decoration:none;| &&
                 |border-radius:3px;padding:2px 7px">&#10003;&nbsp;approve</a>| &&
                 |<a href="sapevent:decline~{ iv_key }"| &&
                 | style="margin-left:8px;background:#922b21;color:#fff;| &&
                 |font-size:11px;font-weight:bold;text-decoration:none;| &&
                 |border-radius:3px;padding:2px 7px">&#10007;&nbsp;decline</a>|.
      endif.
      result = result && render_comment_links( iv_key ) && `</td>`.
    elseif lv_own_hunk = abap_true.
      result = |<td class="cd">...| &&
               |<span style="margin-left:12px;color:#7f8c8d;font-weight:bold">&#9675;&nbsp;own block</span>| &&
               render_comment_links( iv_key ) && `</td>`.
    else.
      result = |<td class="cd">...| &&
               |<a href="sapevent:approve~{ iv_key }"| &&
               | style="margin-left:12px;background:#27ae60;color:#fff;| &&
               |font-size:11px;font-weight:bold;text-decoration:none;| &&
               |border-radius:3px;padding:2px 7px">&#10003;&nbsp;approve</a>| &&
               |<a href="sapevent:decline~{ iv_key }"| &&
               | style="margin-left:8px;background:#922b21;color:#fff;| &&
               |font-size:11px;font-weight:bold;text-decoration:none;| &&
               |border-radius:3px;padding:2px 7px">&#10007;&nbsp;decline</a>| &&
               render_comment_links( iv_key ) && `</td>`.
    endif.
  endmethod.
  method acr_approve_fixed.
    " Returns fixed-position button for diffs without separators
    data(lv_own_hunk) = is_own_hunk( iv_key ).
    data(lv_global_action) = get_hunk_global_action( iv_key ).
    if line_exists( mt_approved[ table_line = iv_key ] ).
      result =
        `<div style="position:fixed;top:8px;right:12px;z-index:999;display:flex;gap:6px;align-items:center">` &&
        `<span style="background:#27ae60;color:#fff;padding:4px 14px;` &&
        `border-radius:4px;font:bold 12px Consolas,sans-serif">&#10003;&nbsp;Approved</span>` &&
        render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'A' ).
      if lv_own_hunk = abap_false.
        result = result &&
          |<a href="sapevent:undo~{ iv_key }"| &&
          ` style="background:#95a5a6;color:#fff;padding:4px 10px;` &&
          `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">Undo</a>`.
      endif.
      result = result && render_comment_links( iv_key ) && `</div>`.
    elseif line_exists( mt_declined[ table_line = iv_key ] ).
      result =
        `<div style="position:fixed;top:8px;right:12px;z-index:999;display:flex;gap:6px;align-items:center">` &&
        `<span style="background:#e74c3c;color:#fff;padding:4px 14px;` &&
        `border-radius:4px;font:bold 12px Consolas,sans-serif">&#10007;&nbsp;Declined</span>` &&
        render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'D' ).
      if lv_own_hunk = abap_false.
        result = result &&
          |<a href="sapevent:undo~{ iv_key }"| &&
          ` style="background:#95a5a6;color:#fff;padding:4px 10px;` &&
          `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">Undo</a>`.
      endif.
      result = result && render_comment_links( iv_key ) && `</div>`.
    elseif lv_global_action = 'A' or lv_global_action = 'D'.
      result =
        `<div style="position:fixed;top:8px;right:12px;z-index:999;display:flex;gap:6px;align-items:center">`.
      if lv_global_action = 'A'.
        result = result &&
          `<span style="background:#27ae60;color:#fff;padding:4px 14px;` &&
          `border-radius:4px;font:bold 12px Consolas,sans-serif">&#10003;&nbsp;Approved</span>` &&
          render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'A' ).
      else.
        result = result &&
          `<span style="background:#e74c3c;color:#fff;padding:4px 14px;` &&
          `border-radius:4px;font:bold 12px Consolas,sans-serif">&#10007;&nbsp;Declined</span>` &&
          render_hunk_action_meta( iv_hunk_key = iv_key iv_action = 'D' ).
      endif.
      if lv_own_hunk = abap_false.
        result = result &&
          |<a href="sapevent:approve~{ iv_key }"| &&
          ` style="background:#27ae60;color:#fff;padding:4px 14px;` &&
          `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">` &&
          `&#10003;&nbsp;Approve</a>` &&
          |<a href="sapevent:decline~{ iv_key }"| &&
          ` style="background:#922b21;color:#fff;padding:4px 14px;` &&
          `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">` &&
          `&#10007;&nbsp;Decline</a>`.
      endif.
      result = result && render_comment_links( iv_key ) && `</div>`.
    elseif lv_own_hunk = abap_true.
      result =
        |<div style="position:fixed;top:8px;right:12px;z-index:999;display:flex;gap:6px">| &&
        `<span style="background:#7f8c8d;color:#fff;padding:4px 14px;` &&
        `border-radius:4px;font:bold 12px Consolas,sans-serif">&#9675;&nbsp;Own Block</span>` &&
        render_comment_links( iv_key ) && `</div>`.
    else.
      result =
        |<div style="position:fixed;top:8px;right:12px;z-index:999;display:flex;gap:6px">| &&
        |<a href="sapevent:approve~{ iv_key }"| &&
        ` style="background:#27ae60;color:#fff;padding:4px 14px;` &&
        `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">` &&
        `&#10003;&nbsp;Approve</a>` &&
        |<a href="sapevent:decline~{ iv_key }"| &&
        ` style="background:#922b21;color:#fff;padding:4px 14px;` &&
        `border-radius:4px;font:bold 12px Consolas,sans-serif;text-decoration:none">` &&
        `&#10007;&nbsp;Decline</a>` &&
        render_comment_links( iv_key ) && `</div>`.
    endif.
  endmethod.
  method on_sapevent.
    check mv_code_review = abap_true.
    data lv_cmd  type string.
    data lv_rest type string.
    data lv_sep_off type i.
    find first occurrence of '~' in action match offset lv_sep_off.
    if sy-subrc <> 0. return. endif.
    lv_cmd = action(lv_sep_off).
    data lv_sep_start type i.
    lv_sep_start = lv_sep_off + 1.
    lv_rest = action+lv_sep_start.
    data lv_scroll_txt type string.
    if lv_cmd = 'openuserdeclined'.
      data lv_scroll_sep type i.
      find first occurrence of '~' in lv_rest match offset lv_scroll_sep.
      if sy-subrc = 0.
        data(lv_tail_start) = lv_scroll_sep + 1.
        data(lv_tail) = lv_rest+lv_tail_start.
        if lv_tail cn '0123456789~'.
          " payload contains another component before the scroll value
        elseif lv_tail ca '~'.
          " keep command-specific parsing below
        elseif lv_tail is not initial.
          lv_scroll_txt = lv_tail.
          lv_rest = lv_rest(lv_scroll_sep).
        endif.
      endif.
    endif.

    if lv_cmd = 'back'.
      back_to_report( ).
      return.

    elseif lv_cmd = 'prepare'.
      prepare_code_review( ).
      return.

    elseif lv_cmd = 'recalcpick'.
      show_recalc_picker( ).
      return.

    elseif lv_cmd = 'prepare_selected'.
      delete_and_recalc_selected( iv_keys = lv_rest ).
      return.

    elseif lv_cmd = 'delete_recalc'.
      delete_and_recalc_selected( iv_keys = lv_rest ).
      return.

    elseif lv_cmd = 'openreview'.
      if open_saved_code_review( ) = abap_false.
        message 'Saved review diff is not available; use Prepare Code Review' type 'S' display like 'E'.
      endif.
      return.

    elseif lv_cmd = 'openobj'.
      " lv_rest = TYPE~OBJNAME~SCROLLY  (TYPE always 4 chars, SCROLLY optional trailing digits)
      data lv_oo_rest type string.
      lv_oo_rest = lv_rest.
      data(lv_rev2) = reverse( lv_oo_rest ).
      data lv_tilde2 type i.
      find first occurrence of '~' in lv_rev2 match offset lv_tilde2.
      if sy-subrc = 0.
        data(lv_scand_start) = strlen( lv_oo_rest ) - lv_tilde2.
        data(lv_scand) = lv_oo_rest+lv_scand_start.
        if lv_scand is not initial and lv_scand co '0123456789'.
          mv_cr_report_scroll = conv i( lv_scand ).
          data(lv_oo_rest_len) = lv_scand_start - 1.
          if lv_oo_rest_len >= 0.
            lv_oo_rest = lv_oo_rest(lv_oo_rest_len).
          endif.
        endif.
      endif.
      " TYPE is always 4 chars
      data lv_oo_type type versobjtyp.
      data lv_oo_name type versobjnam.
      if strlen( lv_oo_rest ) > 5 and lv_oo_rest+4(1) = '~'.
        lv_oo_type = lv_oo_rest(4).
        lv_oo_name = lv_oo_rest+5.
        open_cr_part( iv_objtype = lv_oo_type iv_objname = lv_oo_name ).
      endif.
      return.

    elseif lv_cmd = 'openuserdeclined'.
      show_user_declines( iv_user = conv #( lv_rest ) ).
      return.

    elseif lv_cmd = 'openreviewer'.
      show_user_declines( iv_user = conv #( lv_rest ) iv_reviewer = abap_true ).
      return.

    elseif lv_cmd = 'approveall'.
      " lv_rest = TYPE~OBJNAME — approve all hunks for this object
      data lv_tld2 type i.
      find first occurrence of '~' in lv_rest match offset lv_tld2.
      data lv_nst2 type i.
      lv_nst2 = lv_tld2 + 1.
      data lv_type2  type versobjtyp.
      data lv_onam2  type versobjnam.
      lv_type2 = lv_rest(lv_tld2).
      lv_onam2 = lv_rest+lv_nst2.
      read table mt_acr_stats into data(ls_st2)
        with key objtype = lv_type2 obj_name = lv_onam2.
      if sy-subrc = 0 and ls_st2-hunk_count > 0.
        do ls_st2-hunk_count times.
          data(lv_hk) = |{ lv_rest }~{ sy-index }|.
          check is_own_hunk( lv_hk ) = abap_false.
          insert lv_hk into table mt_approved.
          delete table mt_declined from lv_hk.
          set_hunk_action( iv_hunk_key = lv_hk iv_action = 'A' ).
        enddo.
      endif.

    elseif lv_cmd = 'addcomment' or lv_cmd = 'editreview'.
      data lv_er_key type string.
      lv_er_key = lv_rest.
      clear mv_pending_decline.
      clear mv_pending_edit.
      data(lv_er_note) = ``.
      if lv_cmd = 'editreview'.
        mv_pending_edit = lv_er_key.
        lv_er_note = get_last_own_comment( lv_er_key ).
      endif.
      mo_note_dlg = new zcl_ave_acr_note_dlg(
        iv_title    = lv_er_key
        iv_hunk_key = lv_er_key
        iv_note     = lv_er_note ).
      set handler on_note_dlg_saved for mo_note_dlg.
      set handler on_note_dlg_cancelled for mo_note_dlg.
      mo_note_dlg->show( ).
      return.

    elseif lv_cmd = 'undo'.
      data lv_undo_key type string.
      lv_undo_key = lv_rest.
      if is_own_hunk( lv_undo_key ) = abap_true.
        message 'You cannot undo review status for your own block' type 'S' display like 'E'.
        return.
      endif.
      delete table mt_approved from lv_undo_key.
      delete table mt_declined from lv_undo_key.
      delete table mt_decline_notes with table key hunk_key = lv_undo_key.
      clear_hunk_action( lv_undo_key ).
      if mv_decline_view_user is not initial.
        show_user_declines( iv_user = mv_decline_view_user iv_reviewer = mv_reviewer_view ).
      elseif mv_cr_base_html is not initial and mv_cr_cur_key is not initial.
        set_html( inject_approve_btn( iv_html = mv_cr_base_html iv_key = mv_cr_cur_key ) ).
      endif.
      regen_acr_report( ).
      refresh_rpt_row( ).
      save_review_to_db( iv_silent = abap_true ).
      return.

    elseif lv_cmd = 'approve' or lv_cmd = 'decline'.
      data lv_key type string.
      lv_key = lv_rest.
      if is_own_hunk( lv_key ) = abap_true.
        message 'You cannot approve or decline your own block' type 'S' display like 'E'.
        return.
      endif.
      if lv_cmd = 'approve'.
        insert lv_key into table mt_approved.
        delete table mt_declined from lv_key.
        set_hunk_action( iv_hunk_key = lv_key iv_action = 'A' ).
      else.
        " Open note dialog — decline is registered only when user clicks Save with a comment
        mv_pending_decline = lv_key.
        mo_note_dlg = new zcl_ave_acr_note_dlg(
          iv_title    = lv_key
          iv_hunk_key = lv_key
          iv_note     = `` ).
        set handler on_note_dlg_saved for mo_note_dlg.
        set handler on_note_dlg_cancelled for mo_note_dlg.
        mo_note_dlg->show( ).
        return.  " Decline will be registered in on_note_dlg_saved event
      endif.

      if mv_decline_view_user is not initial.
        show_user_declines( iv_user = mv_decline_view_user iv_reviewer = mv_reviewer_view ).
        regen_acr_report( ).
        refresh_rpt_row( ).
        save_review_to_db( iv_silent = abap_true ).
        return.
      elseif mv_cr_base_html is not initial and mv_cr_cur_key is not initial.
        data(lv_html) = inject_approve_btn(
          iv_html = mv_cr_base_html
          iv_key  = mv_cr_cur_key ).

        " Scroll to the acted chunk by its anchor id
        data(lv_rev) = reverse( lv_key ).
        data lv_tilde_pos type i.
        find first occurrence of '~' in lv_rev match offset lv_tilde_pos.
        if sy-subrc = 0.
          data lv_chunk_start type i.
          lv_chunk_start = strlen( lv_key ) - lv_tilde_pos.
          data(lv_chunk) = lv_key+lv_chunk_start.
          if lv_chunk is not initial.
            data(lv_script) =
              `<script>window.onload=function(){` &&
              `var e=document.getElementById('acr_c` && lv_chunk && `');` &&
              `if(e)e.scrollIntoView({block:'center'});}` &&
              `</script></head>`.
            lv_html = replace( val = lv_html sub = `</head>` with = lv_script ).
          endif.
        endif.

        set_html( lv_html ).
        regen_acr_report( ).
        refresh_rpt_row( ).
        save_review_to_db( iv_silent = abap_true ).
        return.
      endif.
    endif.

    " approveall path (or approve without cached html)
    if mv_decline_view_user is not initial.
      show_user_declines( iv_user = mv_decline_view_user iv_reviewer = mv_reviewer_view ).
    elseif mv_cr_base_html is not initial and mv_cr_cur_key is not initial.
      set_html( inject_approve_btn( iv_html = mv_cr_base_html iv_key = mv_cr_cur_key ) ).
    endif.
    regen_acr_report( ).
    refresh_rpt_row( ).
    save_review_to_db( iv_silent = abap_true ).
  endmethod.
  method maximize_html.
    check mv_focus_html = abap_false.
    mv_focus_html = abap_true.
    mo_toolbar->set_button_info(
      exporting
        fcode = 'FOCUS_TOGGLE'
        text  = 'Standard View'
        icon  = conv #( icon_view_maximize ) ).
    if mv_two_pane = abap_true.
      mo_split_2p_wrap->set_row_height( id = 1 height = 0 ).
      mo_split_2p_wrap->set_row_height( id = 2 height = 100 ).
      mo_split_2p_wrap->set_row_sash( id = 1 type = 0 value = 0 ).
    else.
      mo_split_main->set_column_width( id = 1 width = 0 ).
      mo_split_main->set_column_width( id = 2 width = 100 ).
      mo_split_main->set_column_sash( id = 1 type = 0 value = 0 ).
    endif.
  endmethod.
  method back_to_report.
    clear mv_decline_view_user.
    clear mv_reviewer_view.
    maximize_html( ).
    data(lv_html) = mv_cr_report_html.
    " Scroll to the last opened object row by anchor
    if mv_cr_cur_key is not initial.
      data(lv_anchor) = |obj_{ escape( val = mv_cr_cur_key format = cl_abap_format=>e_html_attr ) }|.
      data(lv_script) =
        `<script>window.onload=function(){` &&
        `var e=document.getElementById('` && lv_anchor && `');` &&
        `if(e)e.scrollIntoView(true);}` &&
        `</script></head>`.
      lv_html = replace( val = lv_html sub = `</head>` with = lv_script ).
    endif.
    set_html( lv_html ).
  endmethod.
  method show_user_declines.
    mv_decline_view_user = iv_user.
    mv_reviewer_view = iv_reviewer.
    data(lv_user_name) = zcl_ave_popup_data=>get_user_name( iv_user ).

    data lt_hunks type standard table of ty_hunk_info with default key.
    if iv_reviewer = abap_true.
      data lt_review_keys type hashed table of string with unique key table_line.
      data ls_review_payload type ty_saved_payload.
      if load_review_payload(
           exporting iv_trkorr = conv #( mv_object_name )
           importing es_payload = ls_review_payload ) = abap_true.
        read table ls_review_payload-user_states into data(ls_review_state)
          with key reviewer = iv_user.
        if sy-subrc = 0.
          loop at ls_review_state-approved into data(ls_review_approved).
            insert ls_review_approved-hunk_key into table lt_review_keys.
          endloop.
          loop at ls_review_state-declined into data(ls_review_declined).
            insert ls_review_declined-hunk_key into table lt_review_keys.
          endloop.
        endif.
      endif.

      if iv_user = sy-uname.
        loop at mt_approved into data(lv_cur_approved_key).
          insert lv_cur_approved_key into table lt_review_keys.
        endloop.
        loop at mt_declined into data(lv_cur_declined_key).
          insert lv_cur_declined_key into table lt_review_keys.
        endloop.
      endif.

      loop at mt_hunk_threads into data(ls_review_thread).
        loop at ls_review_thread-messages transporting no fields where author = iv_user.
          insert ls_review_thread-hunk_key into table lt_review_keys.
          exit.
        endloop.
      endloop.

      loop at lt_review_keys into data(lv_review_key).
        read table mt_hunk_info into data(ls_review_hunk)
          with table key hunk_key = lv_review_key.
        if sy-subrc = 0.
          append ls_review_hunk to lt_hunks.
        endif.
      endloop.
    else.
      loop at mt_hunk_info into data(ls_hi) where author = iv_user.
        append ls_hi to lt_hunks.
      endloop.
    endif.
    sort lt_hunks by class_name objtype obj_name hunk_no.

    data(lv_css) =
      `body{font:13px/1.6 Consolas,monospace;padding:42px 28px 20px 28px;background:#fff;color:#333}` &&
      `h2{color:#2c3e50;border-bottom:2px solid #3498db;padding-bottom:6px;margin-bottom:16px}` &&
      `.objhdr{margin:18px 0 8px 0;background:#dbe9ff;color:#2c3e50;padding:5px 10px;` &&
      `font-weight:bold;white-space:nowrap}` &&
      `.block{margin:0 0 14px 0}` &&
      `.comments{display:block;width:100%;margin:0 0 8px 0}` &&
      `.codewrap{display:block;clear:both;width:100%;margin:0;padding:0}` &&
      `.blame{margin:0 0 6px 0;color:#5e6a75;font-style:italic;white-space:nowrap}` &&
      `.blkinfo{margin:5px 0 2px 0;color:#2c3e50;font-weight:bold;white-space:nowrap}` &&
      `.muted{color:#777;font-weight:normal}` &&
      `.meta{display:block;margin:0 0 4px 0;color:#7f8c99;font-size:10px;font-weight:normal}` &&
      `.note{display:table;margin:6px 0 6px 0;padding:5px 9px;background:#f3f9ff;` &&
      `border:1px solid #a8cde8;color:#155f8f;font-style:italic;font-weight:bold;border-radius:6px}` &&
      `table.diff{border-collapse:collapse;width:100%;font-size:12px;margin:0 0 4px 0}` &&
      `.diff .ln{color:#aaa;text-align:right;padding:1px 10px 1px 5px;` &&
      `min-width:42px;border-right:1px solid #e0e0e0;white-space:nowrap;background:#fafafa}` &&
      `.diff .cd{padding:1px 8px;white-space:pre}` &&
      `.back{position:fixed;top:8px;left:12px;z-index:999;background:#3498db;color:#fff;` &&
      `padding:4px 10px;border-radius:4px;text-decoration:none;font-weight:bold}`.

    data(lv_html) =
      |<!DOCTYPE html><html><head><meta charset="utf-8"><style>{ lv_css }</style></head><body>| &&
      |<a class="back" href="sapevent:back~0">Back</a>| &&
      |<h2>Review: { escape( val = conv string( iv_user ) format = cl_abap_format=>e_html_text ) }| &&
      | / { escape( val = conv string( lv_user_name ) format = cl_abap_format=>e_html_text ) }</h2>|.

    if lt_hunks is initial.
      lv_html = lv_html &&
        cond string(
          when iv_reviewer = abap_true
          then |<p style="color:#888">No reviewed or commented blocks found for this reviewer.</p>|
          else |<p style="color:#888">No changed blocks found for this developer.</p>| ) &&
        |</body></html>|.
      maximize_html( ).
      set_html( lv_html ).
      return.
    endif.

    data lv_cur_obj type string value `####`.
    loop at lt_hunks into data(ls_hunk).
      data(lv_obj_key) = |{ ls_hunk-objtype }~{ ls_hunk-obj_name }|.

      " Object header
      if lv_obj_key <> lv_cur_obj.
        lv_cur_obj = lv_obj_key.
        data(lv_title) = cond string(
          when ls_hunk-class_name is not initial and ls_hunk-display_name is not initial
          then |{ ls_hunk-class_name }=>{ ls_hunk-display_name }|
          when ls_hunk-display_name is not initial then ls_hunk-display_name
          else conv string( ls_hunk-obj_name ) ).
        data lv_obj_blocks  type i.
        data lv_obj_changes type i.
        clear: lv_obj_blocks, lv_obj_changes.
        loop at lt_hunks into data(ls_s) where objtype = ls_hunk-objtype and obj_name = ls_hunk-obj_name.
          lv_obj_blocks  += 1.
          lv_obj_changes += ls_s-change_count.
        endloop.
        lv_html = lv_html &&
          |<div class="objhdr">| &&
          |<a href="sapevent:openobj~{ lv_obj_key }" style="color:inherit;text-decoration:none">| &&
          |{ escape( val = conv string( ls_hunk-objtype ) format = cl_abap_format=>e_html_text ) }: | &&
          |{ escape( val = lv_title format = cl_abap_format=>e_html_text ) }</a>| &&
          | <span class="muted">blocks</span> { lv_obj_blocks }| &&
          | <span class="muted">changes</span> { lv_obj_changes } lines</div>|.
      endif.

      " Hunk diff HTML (same cleanup as before)
      data(lv_clean_html) = ls_hunk-html.
      data lv_mark_pos type i.
      data lv_before_mark type string.
      data lv_after_mark type string.
      data lv_tr_start type i.
      data lv_tr_end_rel type i.
      data lv_tr_end type i.
      data lv_rev_before type string.
      data lv_rev_pos type i.
      while lv_clean_html cs `──</td>`.
        lv_mark_pos = sy-fdpos.
        lv_before_mark = lv_clean_html(lv_mark_pos).
        lv_after_mark = lv_clean_html+lv_mark_pos.
        lv_rev_before = reverse( lv_before_mark ).
        find first occurrence of `rt<` in lv_rev_before match offset lv_rev_pos.
        if sy-subrc <> 0. exit. endif.
        lv_tr_start = strlen( lv_before_mark ) - lv_rev_pos - 3.
        find first occurrence of `</tr>` in lv_after_mark match offset lv_tr_end_rel.
        if sy-subrc <> 0. exit. endif.
        lv_tr_end = lv_mark_pos + lv_tr_end_rel + 5.
        if lv_tr_start < 0 or lv_tr_end <= lv_tr_start. exit. endif.
        lv_clean_html = lv_clean_html(lv_tr_start) && lv_clean_html+lv_tr_end.
      endwhile.
      if lv_clean_html cs `<td class="sep"></td>`.
        data(lv_rows_html) = lv_clean_html.
        data(lv_norm_html) = ``.
        data lv_row_start type i.
        data lv_row_close_rel type i.
        data lv_row_close type i.
        data lv_row_len type i.
        data lv_row_html type string.
        data lv_gt_pos type i.
        data lv_sep_pos type i.
        data lv_body_left type string.
        data lv_body_right type string.
        data lv_plain_left type string.
        data lv_plain_right type string.
        while lv_rows_html cs `<tr`.
          lv_row_start = sy-fdpos.
          if lv_row_start > 0.
            lv_norm_html = lv_norm_html && lv_rows_html(lv_row_start).
            lv_rows_html = lv_rows_html+lv_row_start.
          endif.
          find first occurrence of `</tr>` in lv_rows_html match offset lv_row_close_rel.
          if sy-subrc <> 0.
            lv_norm_html = lv_norm_html && lv_rows_html.
            clear lv_rows_html.
            exit.
          endif.
          lv_row_close = lv_row_close_rel + 5.
          lv_row_html = lv_rows_html(lv_row_close).
          lv_rows_html = lv_rows_html+lv_row_close.
          if lv_row_html cs `<td class="sep"></td>`.
            find first occurrence of `>` in lv_row_html match offset lv_gt_pos.
            find first occurrence of `<td class="sep"></td>` in lv_row_html match offset lv_sep_pos.
            if sy-subrc = 0 and lv_gt_pos >= 0 and lv_sep_pos > lv_gt_pos.
              data(lv_body_left_off)  = lv_gt_pos + 1.
              data(lv_body_left_len)  = lv_sep_pos - lv_gt_pos - 1.
              data(lv_body_right_off) = lv_sep_pos + 21.
              data(lv_row_prefix_len) = lv_gt_pos + 1.
              lv_body_left  = lv_row_html+lv_body_left_off(lv_body_left_len).
              lv_body_right = lv_row_html+lv_body_right_off.
              lv_row_len = strlen( lv_body_right ).
              if lv_row_len >= 5.
                data(lv_body_right_len) = lv_row_len - 5.
                lv_body_right = lv_body_right(lv_body_right_len).
              endif.
              lv_plain_left  = lv_body_left.
              lv_plain_right = lv_body_right.
              replace all occurrences of regex `<[^>]+>` in lv_plain_left  with ``.
              replace all occurrences of regex `<[^>]+>` in lv_plain_right with ``.
              condense lv_plain_left  no-gaps.
              condense lv_plain_right no-gaps.
              lv_norm_html = lv_norm_html &&
                lv_row_html(lv_row_prefix_len) &&
                cond string(
                  when strlen( lv_plain_right ) >= strlen( lv_plain_left )
                  then lv_body_right else lv_body_left ) &&
                `</tr>`.
            else.
              lv_norm_html = lv_norm_html && lv_row_html.
            endif.
          else.
            lv_norm_html = lv_norm_html && lv_row_html.
          endif.
        endwhile.
        lv_clean_html = lv_norm_html && lv_rows_html.
      endif.
      data(lv_code_html) = cond string(
        when lv_clean_html is not initial
        then |<table class="diff"><tbody>{ lv_clean_html }</tbody></table>|
        else `<div style="color:#888;margin:4px 0 10px">Diff not available.</div>` ).

      " Actions (approve / decline / undo / add comment) — same set as in object report
      data(lv_actions_html) = render_hunk_actions_html( ls_hunk-hunk_key ).
      data(lv_block_title) = cond string(
        when ls_hunk-display_name is not initial then ls_hunk-display_name
        else conv string( ls_hunk-obj_name ) ).
      data(lv_change_kind_html) = cond string(
        when ls_hunk-change_kind is not initial
        then | <span class="muted">{ escape( val = ls_hunk-change_kind format = cl_abap_format=>e_html_text ) }</span>|
        else `` ).

      lv_html = lv_html &&
        `<div class="block">` &&
        |<div class="blkinfo">{ escape( val = conv string( ls_hunk-objtype ) format = cl_abap_format=>e_html_text ) }: | &&
        |{ escape( val = lv_block_title format = cl_abap_format=>e_html_text ) } | &&
        |Block #{ ls_hunk-hunk_no }| &&
        lv_change_kind_html &&
        | <span class="muted">line</span> { ls_hunk-start_line }| &&
        | <span class="muted">changes</span> { ls_hunk-change_count }</div>| &&
        lv_actions_html.

      " Comments for this hunk
      data(lv_comments_html) = ``.
      read table mt_hunk_threads into data(ls_thread) with key hunk_key = ls_hunk-hunk_key.
      if sy-subrc = 0.
        loop at ls_thread-messages into data(ls_msg).
          check ls_msg-text is not initial.
          data(lv_note_esc) = escape( val = ls_msg-text format = cl_abap_format=>e_html_text ).
          replace all occurrences of cl_abap_char_utilities=>newline in lv_note_esc with `<br>`.
          data(lv_created_at_txt) = format_timestamp( ls_msg-created_at ).
          data(lv_note_style) = cond string(
            when ls_msg-is_decline = abap_true
            then ` style="background:#fff1f4;border-color:#efb8c8;color:#9f3b57"`
            else `` ).
          lv_comments_html = lv_comments_html &&
            |<span class="meta">{ escape( val = conv string( ls_msg-author ) format = cl_abap_format=>e_html_text ) }| &&
            | / { escape( val = conv string( ls_msg-author_name ) format = cl_abap_format=>e_html_text ) }| &&
            | / { escape( val = lv_created_at_txt format = cl_abap_format=>e_html_text ) }</span>| &&
            |<div class="note"{ lv_note_style }>{ lv_note_esc }</div>|.
        endloop.
      endif.
      if lv_comments_html is not initial.
        lv_html = lv_html && |<div class="comments">{ lv_comments_html }</div>|.
      endif.

      lv_html = lv_html &&
        `<div class="codewrap">` &&
        lv_code_html &&
        `</div></div>`.
    endloop.

    lv_html = lv_html && `</body></html>`.
    maximize_html( ).
    set_html( lv_html ).
  endmethod.
  method open_cr_part.
    " Open the diff for a given type/name — called from report row double-click
    read table mt_acr_stats into data(ls_stat)
      with key objtype = iv_objtype obj_name = iv_objname.
    if sy-subrc <> 0. return. endif.

    data(ls_ck) = value ty_diff_cache_key(
      objtype     = ls_stat-objtype
      objname     = ls_stat-obj_name
      versno_o    = ls_stat-versno_old
      versno_n    = ls_stat-versno_new
      blame       = mv_blame
      two_pane    = mv_two_pane
      compact     = mv_compact
      debug       = mv_debug
      ignore_case = mv_ignore_case ).
    read table mt_diff_cache into data(ls_ch) with table key key = ls_ck.
    if sy-subrc <> 0.
      read table mt_parts into data(ls_part)
        with key type = iv_objtype object_name = iv_objname.
      if sy-subrc <> 0. return. endif.

      mv_cur_objtype   = ls_part-type.
      mv_cur_objname   = ls_part-object_name.
      mv_cur_part_name = cond string(
        when ls_part-class is not initial then |{ ls_part-class } - { ls_part-name }|
        else ls_part-name ).

      load_versions( i_objtype = ls_part-type i_objname = ls_part-object_name ).
      if mt_versions is initial. return. endif.

      clear ms_base_ver.
      if mv_object_type = zcl_ave_object_factory=>gc_type-tr.
        loop at mt_versions into ms_base_ver where korrnum = mv_object_name.
          exit.
        endloop.
      endif.
      if ms_base_ver is initial.
        ms_base_ver = mt_versions[ 1 ].
      endif.
      mv_viewed_versno = ms_base_ver-versno.

      data ls_prev_part type ty_version_row.
      loop at mt_versions into ls_prev_part where versno < ms_base_ver-versno.
        exit.
      endloop.
      update_ver_colors( iv_viewed_versno = mv_viewed_versno ).
      refresh_vers( ).
      if mv_show_diff = abap_true.
        auto_show_diff_or_source( is_old = ls_prev_part is_new = ms_base_ver ).
      else.
        show_source( i_objtype = ms_base_ver-objtype
                     i_objname = ms_base_ver-objname
                     i_versno  = ms_base_ver-versno ).
      endif.
      return.
    endif.

    " Highlight the matching part row in the ALV
    loop at mt_parts assigning field-symbol(<lp>)
      where type = iv_objtype and object_name = iv_objname.
      mv_cur_objtype   = <lp>-type.
      mv_cur_objname   = <lp>-object_name.
      mv_cur_part_name = cond string(
        when <lp>-class is not initial then |{ <lp>-class } – { <lp>-name }|
        else <lp>-name ).
      exit.
    endloop.

    mv_cr_cur_key   = |{ ls_stat-objtype }~{ ls_stat-obj_name }|.
    mv_cr_base_html = ls_ch-html.
    set_html( inject_approve_btn( iv_html = ls_ch-html iv_key = mv_cr_cur_key ) ).
  endmethod.
  method rerender_cr_current.
    result = abap_false.
    check mv_code_review = abap_true.
    check mv_decline_view_user is initial.
    check mv_cr_cur_key is not initial.

    data lv_tld type i.
    find first occurrence of '~' in mv_cr_cur_key match offset lv_tld.
    check sy-subrc = 0.

    data lv_objtype type versobjtyp.
    data lv_objname type versobjnam.
    lv_objtype = mv_cr_cur_key(lv_tld).
    data(lv_name_start) = lv_tld + 1.
    lv_objname = mv_cr_cur_key+lv_name_start.

    read table mt_parts into data(ls_part)
      with key type = lv_objtype object_name = lv_objname.
    check sy-subrc = 0.

    delete mt_acr_stats where objtype = lv_objtype and obj_name = lv_objname.
    delete mt_diff_cache where key-objtype = lv_objtype and key-objname = lv_objname.

    cr_precompute_part( ls_part ).
    open_cr_part( iv_objtype = lv_objtype iv_objname = lv_objname ).
    result = abap_true.
  endmethod.
  method rerender_cr_user_view.
    result = abap_false.
    check mv_code_review = abap_true.
    check mv_decline_view_user is not initial.

    types: begin of ty_obj_key,
             objtype  type versobjtyp,
             obj_name type versobjnam,
           end of ty_obj_key.
    data lt_keys type sorted table of ty_obj_key with unique key objtype obj_name.

    if mv_reviewer_view = abap_true.
      data lt_review_keys type hashed table of string with unique key table_line.
      data ls_review_payload type ty_saved_payload.
      if load_review_payload(
           exporting iv_trkorr = conv #( mv_object_name )
           importing es_payload = ls_review_payload ) = abap_true.
        read table ls_review_payload-user_states into data(ls_review_state)
          with key reviewer = mv_decline_view_user.
        if sy-subrc = 0.
          loop at ls_review_state-approved into data(ls_review_approved).
            insert ls_review_approved-hunk_key into table lt_review_keys.
          endloop.
          loop at ls_review_state-declined into data(ls_review_declined).
            insert ls_review_declined-hunk_key into table lt_review_keys.
          endloop.
        endif.
      endif.

      if mv_decline_view_user = sy-uname.
        loop at mt_approved into data(lv_cur_approved_key).
          insert lv_cur_approved_key into table lt_review_keys.
        endloop.
        loop at mt_declined into data(lv_cur_declined_key).
          insert lv_cur_declined_key into table lt_review_keys.
        endloop.
      endif.

      loop at mt_hunk_threads into data(ls_review_thread).
        loop at ls_review_thread-messages transporting no fields where author = mv_decline_view_user.
          insert ls_review_thread-hunk_key into table lt_review_keys.
          exit.
        endloop.
      endloop.

      loop at lt_review_keys into data(lv_review_key).
        read table mt_hunk_info into data(ls_review_hunk)
          with table key hunk_key = lv_review_key.
        if sy-subrc = 0.
          insert value #( objtype = ls_review_hunk-objtype obj_name = ls_review_hunk-obj_name ) into table lt_keys.
        endif.
      endloop.
    else.
      loop at mt_hunk_info into data(ls_hi) where author = mv_decline_view_user.
        insert value #( objtype = ls_hi-objtype obj_name = ls_hi-obj_name ) into table lt_keys.
      endloop.
    endif.
    check lt_keys is not initial.

    loop at lt_keys into data(ls_key).
      read table mt_parts into data(ls_part)
        with key type = ls_key-objtype object_name = ls_key-obj_name.
      check sy-subrc = 0.

      delete mt_acr_stats where objtype = ls_key-objtype and obj_name = ls_key-obj_name.
      delete mt_diff_cache where key-objtype = ls_key-objtype and key-objname = ls_key-obj_name.
      cr_precompute_part( ls_part ).
    endloop.

    show_user_declines( iv_user = mv_decline_view_user iv_reviewer = mv_reviewer_view ).
    result = abap_true.
  endmethod.
  method on_note_dlg_saved.
    " Called when user clicks Save in the note dialog.
    " For pending decline, register decline; otherwise just add/update comment.
    data lv_msg_ts type timestampl.
    data(lv_is_decline_msg) = xsdbool( mv_pending_decline = iv_hunk_key ).

    if mv_pending_decline = iv_hunk_key and is_own_hunk( iv_hunk_key ) = abap_true.
      clear mv_pending_decline.
      message 'You cannot decline your own block' type 'S' display like 'E'.
      return.
    endif.

    data ls_dn type ty_decline_note.
    ls_dn-hunk_key = iv_hunk_key.
    ls_dn-note     = iv_note.
    insert ls_dn into table mt_decline_notes.
    if sy-subrc <> 0. modify table mt_decline_notes from ls_dn. endif.

    if mv_pending_decline = iv_hunk_key.
      insert iv_hunk_key into table mt_declined.
      delete table mt_approved from iv_hunk_key.
      set_hunk_action( iv_hunk_key = iv_hunk_key iv_action = 'D' ).
    endif.

    read table mt_hunk_threads assigning field-symbol(<ls_thread>)
      with table key hunk_key = iv_hunk_key.
    if sy-subrc <> 0.
      read table mt_hunk_info into data(ls_hunk_info)
        with table key hunk_key = iv_hunk_key.
      if sy-subrc = 0.
        insert value ty_hunk_thread(
          hunk_key     = ls_hunk_info-hunk_key
          objtype      = ls_hunk_info-objtype
          obj_name     = ls_hunk_info-obj_name
          class_name   = ls_hunk_info-class_name
          display_name = ls_hunk_info-display_name
          hunk_no      = ls_hunk_info-hunk_no
          start_line   = ls_hunk_info-start_line
          change_count = ls_hunk_info-change_count
          change_kind  = ls_hunk_info-change_kind
          html         = ls_hunk_info-html ) into table mt_hunk_threads.
        read table mt_hunk_threads assigning <ls_thread>
          with table key hunk_key = iv_hunk_key.
      endif.
    endif.

    if <ls_thread> is assigned.
      get time stamp field lv_msg_ts.
      data(lv_message_handled) = abap_false.
      if mv_pending_edit = iv_hunk_key.
        data(lv_edit_idx) = lines( <ls_thread>-messages ).
        while lv_edit_idx > 0.
          read table <ls_thread>-messages assigning field-symbol(<ls_edit_msg>) index lv_edit_idx.
          if sy-subrc = 0 and <ls_edit_msg>-author = sy-uname.
            <ls_edit_msg>-text = iv_note.
            <ls_edit_msg>-created_at = lv_msg_ts.
            lv_message_handled = abap_true.
            exit.
          endif.
          lv_edit_idx -= 1.
        endwhile.
      endif.

      if lv_message_handled = abap_false.
        read table <ls_thread>-messages into data(ls_last_msg)
          index lines( <ls_thread>-messages ).
        if sy-subrc <> 0
           or ls_last_msg-author <> sy-uname
           or ls_last_msg-is_decline <> lv_is_decline_msg
           or ls_last_msg-text   <> iv_note.
          append value ty_decline_msg(
            author      = sy-uname
            author_name = zcl_ave_popup_data=>get_user_name( sy-uname )
            created_at  = lv_msg_ts
            is_decline  = lv_is_decline_msg
            text        = iv_note ) to <ls_thread>-messages.
        endif.
      endif.
    endif.
    clear mv_pending_decline.
    clear mv_pending_edit.

    save_review_to_db( iv_silent = abap_true ).

    " Refresh diff view and report
    if mv_decline_view_user is not initial.
      show_user_declines( iv_user = mv_decline_view_user iv_reviewer = mv_reviewer_view ).
    elseif mv_cr_base_html is not initial and mv_cr_cur_key is not initial.
      data(lv_html_after_note) = inject_approve_btn(
        iv_html = mv_cr_base_html
        iv_key  = mv_cr_cur_key ).

      data(lv_rev_note) = reverse( iv_hunk_key ).
      data lv_tilde_pos_note type i.
      find first occurrence of '~' in lv_rev_note match offset lv_tilde_pos_note.
      if sy-subrc = 0.
        data lv_chunk_start_note type i.
        lv_chunk_start_note = strlen( iv_hunk_key ) - lv_tilde_pos_note.
        data(lv_chunk_note) = iv_hunk_key+lv_chunk_start_note.
        if lv_chunk_note is not initial.
          data(lv_script_note) =
            `<script>window.onload=function(){` &&
            `var e=document.getElementById('acr_c` && lv_chunk_note && `');` &&
            `if(e)e.scrollIntoView({block:'center'});}` &&
            `</script></head>`.
          lv_html_after_note = replace(
            val  = lv_html_after_note
            sub  = `</head>`
            with = lv_script_note ).
        endif.
      endif.

      set_html( lv_html_after_note ).
    endif.
    refresh_rpt_row( ).
    regen_acr_report( ).
  endmethod.
  method on_note_dlg_cancelled.
    if mv_pending_decline = iv_hunk_key.
      clear mv_pending_decline.
    endif.
    if mv_pending_edit = iv_hunk_key.
      clear mv_pending_edit.
    endif.
  endmethod.
  method regen_acr_report.
    if mv_cr_prepared = abap_true.
      sanitize_review_state( ).
      data lt_report_approved type zif_ave_acr_types=>ty_approved.
      data lt_report_declined type zif_ave_acr_types=>ty_approved.
      collect_report_status(
        importing
          et_approved = lt_report_approved
          et_declined = lt_report_declined ).
      mv_cr_report_html = zcl_ave_acr_report=>to_html(
        it_obj_stats = mt_acr_stats
        it_approved  = lt_report_approved
        it_declined  = lt_report_declined
        it_reviewers = get_reviewer_stats( )
        i_korrnum    = conv #( mv_object_name ) ).
    else.
      mv_cr_report_html = build_cr_object_report_html( ).
    endif.
  endmethod.
  method build_cr_object_report_html.
    data lv_korr_text type as4text.
    data(lv_korrnum) = conv trkorr( mv_object_name ).
    select single as4text from e07t
      where trkorr = @lv_korrnum and langu = @sy-langu
      into @lv_korr_text.

    data(lv_css) =
      `body{font:13px/1.6 Consolas,monospace;padding:20px 28px;background:#fff;color:#333}` &&
      `h2{color:#2c3e50;border-bottom:2px solid #3498db;padding-bottom:6px;margin-bottom:16px}` &&
      `.prepare{text-align:center;margin:8px 0 18px 0}` &&
      `.prepare a{display:inline-block;background:#27ae60;color:#fff;text-decoration:none;` &&
      `font:bold 13px Consolas,monospace;border-radius:4px;padding:7px 20px}` &&
      `table{border-collapse:collapse;width:100%;margin-bottom:16px;font-size:12px}` &&
      `th{background:#3498db;color:#fff;padding:5px 10px;text-align:left;white-space:nowrap}` &&
      `td{padding:4px 10px;border-bottom:1px solid #eee;white-space:nowrap}` &&
      `tr:hover td{background:#f5f9ff}` &&
      `.nr{text-align:right}.muted{color:#777}`.

    data(lv_has_saved_review) = abap_false.
    if has_review_table( ) = abap_true.
      data(ls_saved_payload_check) = value ty_saved_payload( ).
      if load_review_payload(
           exporting iv_trkorr = conv #( mv_object_name )
           importing es_payload = ls_saved_payload_check ) = abap_true
         and ls_saved_payload_check-obj_stats is not initial
         and ls_saved_payload_check-hunks is not initial
         and ls_saved_payload_check-diff_cache is not initial.
        lv_has_saved_review = abap_true.
      endif.
    endif.

    types: begin of ty_cr_rele_task,
             trkorr type trkorr,
             owner  type versuser,
             datum  type versdate,
             zeit   type verstime,
           end of ty_cr_rele_task.
    types: begin of ty_cr_author_key,
             author type versuser,
           end of ty_cr_author_key.
    types: begin of ty_cr_task_key,
             trkorr type trkorr,
           end of ty_cr_task_key.
    types: begin of ty_cr_task_object,
             trkorr   type trkorr,
             object   type e071-object,
             obj_name type e071-obj_name,
             owner    type versuser,
             datum    type versdate,
             zeit     type verstime,
           end of ty_cr_task_object.
    data lt_cr_rele_tasks type standard table of ty_cr_rele_task with default key.
    data lt_cr_task_objects type standard table of ty_cr_task_object with default key.
    data lv_cr_corr_pgmid type e071-pgmid value 'CORR'.
    data lv_cr_corr_rele  type e071-object value 'RELE'.

    select obj_name from e071
      where trkorr = @lv_korrnum
        and pgmid  = @lv_cr_corr_pgmid
        and object = @lv_cr_corr_rele
      into table @data(lt_cr_rele_objects).

    loop at lt_cr_rele_objects into data(lv_cr_rele_obj).
      data lv_cr_task_text  type string.
      data lv_cr_date_text  type string.
      data lv_cr_time_text  type string.
      data lv_cr_owner_text type string.
      condense lv_cr_rele_obj.
      split lv_cr_rele_obj at space
        into lv_cr_task_text lv_cr_date_text lv_cr_time_text lv_cr_owner_text.
      check lv_cr_task_text is not initial
        and strlen( lv_cr_date_text ) = 8
        and strlen( lv_cr_time_text ) = 6
        and lv_cr_date_text co '0123456789'
        and lv_cr_time_text co '0123456789'.
      append value #(
        trkorr = lv_cr_task_text
        owner  = lv_cr_owner_text
        datum  = lv_cr_date_text
        zeit   = lv_cr_time_text ) to lt_cr_rele_tasks.
    endloop.

    if lt_cr_rele_tasks is not initial.
      select trkorr, object, obj_name from e071
        for all entries in @lt_cr_rele_tasks
        where trkorr = @lt_cr_rele_tasks-trkorr
        into table @data(lt_cr_e071_objects).
      loop at lt_cr_e071_objects into data(ls_cr_e071_object).
        read table lt_cr_rele_tasks into data(ls_cr_rele_meta)
          with key trkorr = ls_cr_e071_object-trkorr.
        check sy-subrc = 0.
        append value #(
          trkorr   = ls_cr_e071_object-trkorr
          object   = ls_cr_e071_object-object
          obj_name = ls_cr_e071_object-obj_name
          owner    = ls_cr_rele_meta-owner
          datum    = ls_cr_rele_meta-datum
          zeit     = ls_cr_rele_meta-zeit ) to lt_cr_task_objects.
      endloop.
    endif.

    result =
      |<!DOCTYPE html><html><head><meta charset="utf-8">| &&
      |<style>{ lv_css }</style></head><body>| &&
      |<h2>&#128196;&nbsp;Code Review Report&nbsp;-&nbsp;| &&
      |<span style="color:#3498db">{ escape( val = conv string( mv_object_name ) format = cl_abap_format=>e_html_text ) }|.
    if lv_korr_text is not initial.
      result = result && |&nbsp;-&nbsp;{ escape( val = conv string( lv_korr_text ) format = cl_abap_format=>e_html_text ) }|.
    endif.
    result = result && |</span></h2>|.

    result = result && `<div class="prepare">`.
    if lv_has_saved_review = abap_true.
      result = result &&
        `<a href="sapevent:openreview~0">Open Review</a>` &&
        `&nbsp;&nbsp;` &&
        `<a href="sapevent:recalcpick~0" style="background:#7f8c8d">Recalc Diff</a>`.
    else.
      result = result &&
        `<a href="sapevent:prepare~0">Prepare Code Review</a>`.
    endif.
    result = result &&
      `</div>` &&
      |<table><tr>| &&
      |<th>Type</th><th>Object</th><th>Class</th><th>Type Description</th>| &&
      |<th>Author</th><th class="nr">Tasks</th><th>Start</th><th>Finish</th><th class="nr">Days</th>| &&
      |<th class="nr">Rows</th></tr>|.

    loop at mt_parts into data(ls_part) where type <> 'RPT'.
      data(lv_objname_str) = conv string( ls_part-object_name ).
      " Key: fixed-width TYPE (4 chars) + OBJNAME — no ~ separator in name possible
      data(lv_part_key) = |{ ls_part-type }~{ lv_objname_str }|.
      data lv_part_authors type string.
      data lv_part_task_count type i.
      data lv_part_first_date type versdate.
      data lv_part_last_date type versdate.
      clear: lv_part_authors, lv_part_task_count, lv_part_first_date, lv_part_last_date.

      if lt_cr_task_objects is not initial.
        data lv_part_e071_type type e071-object.
        data lv_part_e071_name type e071-obj_name.
        lv_part_e071_type = switch e071-object( ls_part-type
          when 'REPS' or 'REPT'                                then 'PROG'
          when 'CINC' or 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' then 'CLAS'
          else ls_part-type ).
        lv_part_e071_name = ls_part-object_name.
        if lv_part_e071_type = 'CLAS' and ls_part-class is not initial.
          lv_part_e071_name = ls_part-class.
        elseif lv_part_e071_type = 'CLAS' and lv_part_e071_name cs '='.
          data(lv_part_eq) = find( val = conv string( lv_part_e071_name ) sub = '=' ).
          if lv_part_eq > 0.
            lv_part_e071_name = lv_part_e071_name(lv_part_eq).
          endif.
        endif.

        data lt_part_authors type sorted table of ty_cr_author_key with unique key author.
        data lt_part_tasks type sorted table of ty_cr_task_key with unique key trkorr.
        clear: lt_part_authors, lt_part_tasks.
        loop at lt_cr_task_objects into data(ls_cr_task_object).
          data(lv_touched) = xsdbool(
            ls_cr_task_object-object = lv_part_e071_type
            and ls_cr_task_object-obj_name = lv_part_e071_name ).
          if lv_touched = abap_false
             and lv_part_e071_type = 'PROG'
             and ls_cr_task_object-object = 'REPS'
             and ls_cr_task_object-obj_name = lv_part_e071_name.
            lv_touched = abap_true.
          endif.
          check lv_touched = abap_true.

          insert value #( trkorr = ls_cr_task_object-trkorr ) into table lt_part_tasks.
          if ls_cr_task_object-owner is not initial.
            insert value #( author = ls_cr_task_object-owner ) into table lt_part_authors.
          endif.
          if lv_part_first_date is initial or ls_cr_task_object-datum < lv_part_first_date.
            lv_part_first_date = ls_cr_task_object-datum.
          endif.
          if lv_part_last_date is initial or ls_cr_task_object-datum > lv_part_last_date.
            lv_part_last_date = ls_cr_task_object-datum.
          endif.
        endloop.

        lv_part_task_count = lines( lt_part_tasks ).
        loop at lt_part_authors into data(ls_part_author).
          data(lv_part_author_name) = zcl_ave_popup_data=>get_user_name( ls_part_author-author ).
          if lv_part_author_name is initial.
            lv_part_author_name = ls_part_author-author.
          endif.
          if lv_part_authors is initial.
            lv_part_authors = lv_part_author_name.
          else.
            lv_part_authors = lv_part_authors && `, ` && lv_part_author_name.
          endif.
        endloop.
      endif.

      data lv_start_date type string.
      data lv_finish_date type string.
      data lv_days type i.
      clear: lv_start_date, lv_finish_date, lv_days.
      if lv_part_first_date is not initial.
        lv_start_date = conv string( lv_part_first_date ).
        lv_start_date = |{ lv_start_date+6(2) }.{ lv_start_date+4(2) }.{ lv_start_date+2(2) }|.
      endif.
      if lv_part_last_date is not initial.
        lv_finish_date = conv string( lv_part_last_date ).
        lv_finish_date = |{ lv_finish_date+6(2) }.{ lv_finish_date+4(2) }.{ lv_finish_date+2(2) }|.
      endif.
      if lv_part_first_date is not initial and lv_part_last_date is not initial.
        lv_days = lv_part_last_date - lv_part_first_date + 1.
      endif.

      result = result &&
        |<tr>| &&
        |<td>{ escape( val = conv string( ls_part-type ) format = cl_abap_format=>e_html_text ) }</td>| &&
        |<td><b>{ escape( val = condense( val = lv_objname_str ) format = cl_abap_format=>e_html_text ) }</b></td>| &&
        |<td>{ escape( val = conv string( ls_part-class ) format = cl_abap_format=>e_html_text ) }</td>| &&
        |<td>{ escape( val = conv string( ls_part-type_text ) format = cl_abap_format=>e_html_text ) }</td>| &&
        |<td>{ escape( val = lv_part_authors format = cl_abap_format=>e_html_text ) }</td>| &&
        |<td class="nr">{ lv_part_task_count }</td>| &&
        |<td>{ lv_start_date }</td>| &&
        |<td>{ lv_finish_date }</td>| &&
        |<td class="nr">{ lv_days }</td>| &&
        |<td class="nr">{ ls_part-rows }</td>| &&
        |</tr>|.
    endloop.

    data(lv_obj_count) = lines( mt_parts ).
    if line_exists( mt_parts[ type = 'RPT' ] ).
      lv_obj_count = lv_obj_count - 1.
    endif.
    if lv_obj_count = 0.
      result = result &&
        |<tr><td colspan="9" class="muted">No changed objects found.</td></tr>|.
    endif.

    result = result && |</table></body></html>|.
  endmethod.
  method prepare_code_review.
    check mv_code_review = abap_true.

    data(lv_selected_only) = xsdbool( iv_keys is not initial and iv_keys <> `0` ).
    data lt_selected_keys type hashed table of string with unique key table_line.
    if lv_selected_only = abap_true.
      split iv_keys at `;` into table data(lt_selected_raw).
      loop at lt_selected_raw into data(lv_selected_raw).
        check lv_selected_raw is not initial.
        insert lv_selected_raw into table lt_selected_keys.
      endloop.
    endif.

    clear: mv_cr_base_html, mv_cr_cur_key, mv_decline_view_user.
    if lv_selected_only = abap_true.
      clear: mt_acr_stats, mt_hunk_info, mt_hunk_threads, mt_diff_cache,
             mt_approved, mt_declined, mt_decline_notes.
      load_review_from_db( ).
    else.
      clear: mt_acr_stats, mt_hunk_info, mt_hunk_threads, mt_diff_cache,
             mt_approved, mt_declined, mt_decline_notes.
    endif.

    mv_cr_prepared = abap_true.
    maximize_html( ).

    data lv_total type i.
    loop at mt_parts into data(ls_total_part) where type <> 'RPT'.
      data(lv_total_key) = |{ ls_total_part-type }~{ ls_total_part-object_name }|.
      if lv_selected_only = abap_true
         and not line_exists( lt_selected_keys[ table_line = lv_total_key ] ).
        continue.
      endif.
      lv_total += 1.
    endloop.
    data lv_done type i.

    loop at mt_parts into data(ls_part) where type <> 'RPT'.
      data(lv_part_key) = |{ ls_part-type }~{ ls_part-object_name }|.
      if lv_selected_only = abap_true
         and not line_exists( lt_selected_keys[ table_line = lv_part_key ] ).
        continue.
      endif.
      lv_done += 1.
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = conv i( lv_done * 100 / cond i( when lv_total > 0 then lv_total else 1 ) )
          text       = conv char70( |Code Review: preparing { ls_part-object_name }| ).
      if ls_part-type = 'CLAS'.
        delete mt_acr_stats where class_name = ls_part-object_name.
        delete mt_hunk_info where class_name = ls_part-object_name.
        delete mt_diff_cache where key-objname = ls_part-object_name.
        cr_precompute_class_parts( conv #( ls_part-object_name ) ).
      else.
        delete mt_acr_stats where objtype = ls_part-type and obj_name = ls_part-object_name.
        delete mt_hunk_info where objtype = ls_part-type and obj_name = ls_part-object_name.
        delete mt_diff_cache where key-objtype = ls_part-type and key-objname = ls_part-object_name.
        cr_precompute_part( ls_part ).
      endif.

      sanitize_review_state( ).
      data lt_report_approved type zif_ave_acr_types=>ty_approved.
      data lt_report_declined type zif_ave_acr_types=>ty_approved.
      collect_report_status(
        importing
          et_approved = lt_report_approved
          et_declined = lt_report_declined ).
      mv_cr_report_html = zcl_ave_acr_report=>to_html(
        it_obj_stats = mt_acr_stats
        it_approved  = lt_report_approved
        it_declined  = lt_report_declined
        it_reviewers = get_reviewer_stats( )
        i_korrnum    = conv #( mv_object_name ) ).
      set_html( mv_cr_report_html ).
      cl_gui_cfw=>flush( exceptions others = 1 ).
    endloop.

    load_review_from_db( ).
    regen_acr_report( ).
    refresh_rpt_row( ).
    save_review_to_db( iv_silent = abap_true ).
    set_html( mv_cr_report_html ).
  endmethod.
  method delete_and_recalc_selected.
    check mv_code_review = abap_true.
    check iv_keys is not initial.

    data lt_selected_keys type hashed table of string with unique key table_line.
    split iv_keys at `;` into table data(lt_selected_raw).
    loop at lt_selected_raw into data(lv_selected_raw).
      check lv_selected_raw is not initial.
      insert lv_selected_raw into table lt_selected_keys.
    endloop.

    data(lv_selectable_count) = 0.
    data(lv_all_selected) = abap_true.
    loop at mt_parts into data(ls_part_all_check) where type <> 'RPT'.
      lv_selectable_count += 1.
      data(lv_part_all_key) = |{ ls_part_all_check-type }~{ ls_part_all_check-object_name }|.
      if not line_exists( lt_selected_keys[ table_line = lv_part_all_key ] ).
        lv_all_selected = abap_false.
      endif.
    endloop.

    if lv_selectable_count > 0
       and lv_all_selected = abap_true
       and lines( lt_selected_keys ) >= lv_selectable_count.
      if has_review_table( ) = abap_true.
        data(lv_tabname_del) = conv tabname( 'ZAVE_REVIEW' ).
        data(lv_trkorr_del) = conv trkorr( mv_object_name ).
        try.
            delete from (lv_tabname_del) where trkorr = @lv_trkorr_del.
          catch cx_sy_dynamic_osql_semantics
                cx_sy_dynamic_osql_syntax
                cx_sy_open_sql_db.
        endtry.
      endif.
      clear: mt_acr_stats, mt_hunk_info, mt_hunk_threads, mt_diff_cache,
             mt_approved, mt_declined, mt_decline_notes, mt_hunk_actions.
      prepare_code_review( ).
      return.
    endif.

    load_review_from_db( ).

    loop at mt_parts into data(ls_part_stat) where type <> 'RPT'.
      data(lv_part_stat_key) = |{ ls_part_stat-type }~{ ls_part_stat-object_name }|.
      check line_exists( lt_selected_keys[ table_line = lv_part_stat_key ] ).
      if ls_part_stat-type = 'CLAS'.
        delete mt_acr_stats where class_name = ls_part_stat-object_name.
      else.
        delete mt_acr_stats where objtype = ls_part_stat-type and obj_name = ls_part_stat-object_name.
      endif.
    endloop.

    data lt_hunk_keys_to_delete type hashed table of string with unique key table_line.
    loop at mt_hunk_info into data(ls_hunk_to_check).
      loop at mt_parts into data(ls_part) where type <> 'RPT'.
        data(lv_part_key) = |{ ls_part-type }~{ ls_part-object_name }|.
        if not line_exists( lt_selected_keys[ table_line = lv_part_key ] ).
          continue.
        endif.
        if ls_part-type = 'CLAS'.
          if ls_hunk_to_check-class_name = ls_part-object_name.
            insert ls_hunk_to_check-hunk_key into table lt_hunk_keys_to_delete.
          endif.
        else.
          if ls_hunk_to_check-objtype = ls_part-type and ls_hunk_to_check-obj_name = ls_part-object_name.
            insert ls_hunk_to_check-hunk_key into table lt_hunk_keys_to_delete.
          endif.
        endif.
      endloop.
    endloop.

    loop at lt_hunk_keys_to_delete into data(lv_hunk_key).
      delete table mt_approved from lv_hunk_key.
      delete table mt_declined from lv_hunk_key.
      delete mt_hunk_info where hunk_key = lv_hunk_key.
      delete mt_decline_notes where hunk_key = lv_hunk_key.
      delete mt_hunk_threads where hunk_key = lv_hunk_key.
      delete mt_hunk_actions where hunk_key = lv_hunk_key.
    endloop.

    loop at mt_parts into data(ls_part_clean) where type <> 'RPT'.
      data(lv_part_clean_key) = |{ ls_part_clean-type }~{ ls_part_clean-object_name }|.
      check line_exists( lt_selected_keys[ table_line = lv_part_clean_key ] ).
      if ls_part_clean-type = 'CLAS'.
        delete mt_diff_cache where key-objname = ls_part_clean-object_name.
      else.
        delete mt_diff_cache where key-objtype = ls_part_clean-type and key-objname = ls_part_clean-object_name.
      endif.
    endloop.

    sanitize_review_state( ).
    save_review_to_db( iv_silent = abap_true ).
    prepare_code_review( iv_keys = iv_keys ).
  endmethod.
  method show_recalc_picker.
    data(ls_payload) = value ty_saved_payload( ).
    data(lv_has_payload) = load_review_payload(
      exporting
        iv_trkorr  = conv #( mv_object_name )
      importing
        es_payload = ls_payload ).

    data(lv_css) =
      `body{font:13px/1.6 Consolas,monospace;padding:20px 28px;background:#fff;color:#333}` &&
      `h2{color:#2c3e50;border-bottom:2px solid #3498db;padding-bottom:6px;margin-bottom:16px}` &&
      `table{border-collapse:collapse;width:100%;margin-bottom:16px;font-size:12px}` &&
      `th{background:#3498db;color:#fff;padding:5px 10px;text-align:left;white-space:nowrap}` &&
      `td{padding:4px 10px;border-bottom:1px solid #eee;white-space:nowrap}` &&
      `.go{display:inline-block;background:#7f8c8d;color:#fff;text-decoration:none;` &&
      `font:bold 13px Consolas,monospace;border-radius:4px;padding:7px 20px}` &&
      `.back{display:inline-block;background:#3498db;color:#fff;text-decoration:none;` &&
      `font:bold 13px Consolas,monospace;border-radius:4px;padding:7px 14px;margin-left:8px}` &&
      `.clear{display:inline-block;background:#95a5a6;color:#fff;text-decoration:none;` &&
      `font:bold 13px Consolas,monospace;border-radius:4px;padding:7px 14px;margin-left:8px}` &&
      `.new{color:#27ae60;font-weight:bold}.cached{color:#777}`.

    data(lv_html) =
      |<!DOCTYPE html><html><head><meta charset="utf-8"><style>{ lv_css }</style>| &&
      `<script>` &&
      `function go(){var xs=document.querySelectorAll('input[name=o]:checked');` &&
      `var a=[];for(var i=0;i<xs.length;i++){a.push(xs[i].value);}` &&
      `if(a.length==0){alert('Select at least one object');return false;}` &&
      `location.href='sapevent:prepare_selected~'+a.join(';');return false;}` &&
      `function del_recalc(){var xs=document.querySelectorAll('input[name=o]:checked');` &&
      `var a=[];for(var i=0;i<xs.length;i++){a.push(xs[i].value);}` &&
      `if(a.length==0){alert('Select at least one object');return false;}` &&
      `location.href='sapevent:delete_recalc~'+a.join(';');return false;}` &&
      `function allc(v){var xs=document.querySelectorAll('input[name=o]');` &&
      `for(var i=0;i<xs.length;i++){xs[i].checked=v;}}` &&
      `</script></head><body>` &&
      |<h2>Recalc Diff - { escape( val = conv string( mv_object_name ) format = cl_abap_format=>e_html_text ) }</h2>| &&
      `<p><a class="go" href="#" onclick="return go()">Recalc Selected</a>` &&
      `&nbsp;<a class="go" style="background:#e74c3c" href="#" onclick="return del_recalc()">Delete and recalc</a>` &&
      `<a class="back" href="sapevent:back~0">Back</a>` &&
      `<a class="clear" href="#" onclick="allc(false);return false">Clear Selected</a>` &&
      `&nbsp;&nbsp;<a href="#" onclick="allc(true);return false">Select all</a>` &&
      `</p>` &&
      `<table><tr><th></th><th>Type</th><th>Object</th><th>Class</th><th>Status</th><th class="nr">Rows</th></tr>`.

    loop at mt_parts into data(ls_part) where type <> 'RPT'.
      data(lv_key) = |{ ls_part-type }~{ ls_part-object_name }|.
      data(lv_cached) = abap_false.
      if lv_has_payload = abap_true.
        read table ls_payload-obj_stats transporting no fields
          with key objtype = ls_part-type obj_name = ls_part-object_name.
        lv_cached = xsdbool( sy-subrc = 0 ).
      endif.
      data(lv_status) = cond string(
        when lv_cached = abap_true then `<span class="cached">cached</span>`
        else `<span class="new">new</span>` ).
      lv_html = lv_html &&
        `<tr>` &&
        |<td><input type="checkbox" name="o" checked value="{ escape( val = lv_key format = cl_abap_format=>e_html_attr ) }"></td>| &&
        |<td>{ escape( val = conv string( ls_part-type ) format = cl_abap_format=>e_html_text ) }</td>| &&
        |<td><b>{ escape( val = conv string( ls_part-object_name ) format = cl_abap_format=>e_html_text ) }</b></td>| &&
        |<td>{ escape( val = conv string( ls_part-class ) format = cl_abap_format=>e_html_text ) }</td>| &&
        |<td>{ lv_status }</td>| &&
        |<td class="nr">{ ls_part-rows }</td>| &&
        `</tr>`.
    endloop.

    lv_html = lv_html && `</table></body></html>`.
    maximize_html( ).
    set_html( lv_html ).
  endmethod.
  method open_saved_code_review.
    result = abap_false.
    check mv_code_review = abap_true.
    check mv_object_type = zcl_ave_object_factory=>gc_type-tr.
    check has_review_table( ) = abap_true.

    data(ls_payload) = value ty_saved_payload( ).
    check load_review_payload(
      exporting iv_trkorr = conv #( mv_object_name )
      importing es_payload = ls_payload ) = abap_true.
    check ls_payload-obj_stats is not initial.
    check ls_payload-hunks is not initial.
    check ls_payload-diff_cache is not initial.

    clear: mt_acr_stats, mt_hunk_info, mt_hunk_threads, mt_diff_cache,
           mt_approved, mt_declined, mt_decline_notes,
           mv_cr_base_html, mv_cr_cur_key, mv_decline_view_user,
           mv_reviewer_view.

    mt_acr_stats = ls_payload-obj_stats.
    mt_hunk_info = ls_payload-hunks.
    mt_diff_cache = ls_payload-diff_cache.
    mv_cr_prepared = abap_true.

    load_review_from_db( ).
    regen_acr_report( ).
    refresh_rpt_row( ).
    maximize_html( ).
    set_html( mv_cr_report_html ).
    result = abap_true.
  endmethod.
  method refresh_rpt_row.
    data(lv_approved) = lines( mt_approved ).
    data(lv_obj_count) = lines( mt_parts ).
    if line_exists( mt_parts[ type = 'RPT' ] ).
      lv_obj_count = lv_obj_count - 1.
    endif.
    data(lv_name) = cond string(
      when mv_cr_prepared = abap_true
      then |[ Code Review Report - { lv_approved } hunk(s) approved ]|
      else |[ Code Review Report - { lv_obj_count } object(s) ]| ).
    loop at mt_parts assigning field-symbol(<rpt>) where type = 'RPT'.
      <rpt>-name = lv_name.
      exit.
    endloop.
    refresh_parts( ).
  endmethod.
endclass.

class zcl_ave_object_tr implementation.
  method constructor.
    me->id = id.
  endmethod.
  method get_object.
    try.
        result = cond #(
          " R3TR CLAS → single row (drill-in via double-click)
          when object_key-pgmid = 'R3TR' and object_key-object = 'CLAS'
            then new zcl_ave_object_clas( conv #( object_key-obj_name ) )
          when object_key-pgmid = 'R3TR' and object_key-object = 'INTF'
            then new zcl_ave_object_intf( conv #( object_key-obj_name ) )
          " R3TR PROG → program
          when object_key-pgmid = 'R3TR' and object_key-object = 'PROG'
            then new zcl_ave_object_prog( conv #( object_key-obj_name ) )
          " R3TR FUGR → function group main include
*          WHEN object_key-pgmid = 'R3TR' AND object_key-object = 'FUGR'
*            THEN NEW zcl_ave_object_prog( CONV #( object_key-obj_name ) )
          " LIMU FUNC → single function module
          when object_key-pgmid = 'LIMU' and object_key-object = 'FUNC'
            then new zcl_ave_object_func( conv #( object_key-obj_name ) )
          " LIMU REPS → single program/include
          when object_key-pgmid = 'LIMU' and object_key-object = 'REPS'
            then new zcl_ave_object_prog( conv #( object_key-obj_name ) ) ).
      catch zcx_ave.
        clear result.
    endtry.
  endmethod.
  method get_object_keys.
    data request_data type trwbo_request.
    request_data-h-trkorr = id.

    call function 'TRINT_READ_REQUEST'
      exporting
        iv_read_objs  = abap_true
      changing
        cs_request    = request_data
      exceptions
        error_occured = 1
        others        = 2.
    if sy-subrc <> 0.
      raise exception type zcx_ave.
    endif.

    result = request_data-objects.
    sort result by pgmid ascending object ascending obj_name ascending.
    delete adjacent duplicates from result comparing pgmid object obj_name.
  endmethod.
  method get_objects_for_keys.
    result = value #(
      for key in object_keys
      let obj = get_object( key )
      in ( obj ) ).
    delete result where table_line is not bound.
  endmethod.
  method zif_ave_object~check_exists.
    try.
        new zcl_ave_request( me->id ).
        result = abap_true.
      catch zcx_ave.
        result = abap_false.
    endtry.
  endmethod.
  method zif_ave_object~get_name.
    result = id.
  endmethod.
  method zif_ave_object~get_parts.
    loop at get_object_keys( ) into data(key).
      if key-pgmid = 'R3TR' and ( key-object = 'CLAS' or key-object = 'INTF' ).
        " CLAS/INTF is shown as a single row; double-click opens the object-level popup
        append value #(
          unit        = conv string( key-obj_name )
          object_name = conv versobjnam( key-obj_name )
          type        = conv versobjtyp( key-object ) ) to result.
      elseif key-pgmid = 'LIMU' and key-object = 'METH'.
        " METH: obj_name may be CLASSNAME\METHODNAME or just METHODNAME
        data lv_meth_cls  type seoclsname.
        data lv_meth_name type seocmpname.
        data lv_meth_raw  type string.
        lv_meth_raw = key-obj_name.
        condense lv_meth_raw.
        split lv_meth_raw at ` ` into data(lv_cls_part) data(lv_meth_part).
        lv_meth_cls  = lv_cls_part.
        lv_meth_name = lv_meth_part.
        append value #(
          class       = conv string( lv_meth_cls )
          unit        = conv string( lv_meth_name )
          object_name = conv versobjnam( |{ lv_meth_cls width = 30 }{ lv_meth_name }| )
          type        = 'METH' ) to result.
        clear: lv_meth_cls, lv_meth_name, lv_meth_raw.
      else.
        data(obj) = get_object( key ).
        if obj is bound.
          append lines of obj->get_parts( ) to result.
        else.
          " Unknown/unsupported type — show as-is so it's not silently dropped
          append value #(
            unit        = conv string( key-obj_name )
            object_name = conv versobjnam( key-obj_name )
            type        = conv versobjtyp( key-object ) ) to result.
        endif.
      endif.
    endloop.
  endmethod.
endclass.

class zcl_ave_object_prog implementation.

  method constructor.
    me->name = name.
  endmethod.

  method zif_ave_object~check_exists.
    select single @abap_true into @result
      from trdir
      where name = @name.
  endmethod.

  method zif_ave_object~get_name.
    result = name.
  endmethod.

  method zif_ave_object~get_parts.
    result = value #( (
      unit        = conv #( name )
      object_name = conv #( name )
      type        = 'REPS' ) ).
  endmethod.

endclass.

class zcl_ave_object_pack implementation.

  method constructor.
    me->id = id.
  endmethod.

  method get_object.
    try.
        result = cond #(
          when object_key-pgmid = 'R3TR' and object_key-object = 'CLAS'
            then new zcl_ave_object_clas( conv #( object_key-obj_name ) )
          when object_key-pgmid = 'R3TR' and object_key-object = 'INTF'
            then new zcl_ave_object_intf( conv #( object_key-obj_name ) )
          when object_key-pgmid = 'R3TR' and object_key-object = 'PROG'
            then new zcl_ave_object_prog( conv #( object_key-obj_name ) )
          when object_key-pgmid = 'R3TR' and object_key-object = 'FUGR'
            then new zcl_ave_object_prog( conv #( object_key-obj_name ) )
          when object_key-pgmid = 'LIMU' and object_key-object = 'FUNC'
            then new zcl_ave_object_func( conv #( object_key-obj_name ) )
          when object_key-pgmid = 'LIMU' and object_key-object = 'REPS'
            then new zcl_ave_object_prog( conv #( object_key-obj_name ) ) ).
      catch zcx_ave.
        clear result.
    endtry.
  endmethod.

  method get_object_keys.
    data lt_tadir type standard table of tadir.
    select pgmid, object, obj_name from tadir
      where devclass = @me->id
      into corresponding fields of table @lt_tadir.
    if sy-subrc <> 0.
      raise exception type zcx_ave.
    endif.
    loop at lt_tadir into data(ls_tadir).
      append value trwbo_s_e071(
        pgmid    = ls_tadir-pgmid
        object   = ls_tadir-object
        obj_name = ls_tadir-obj_name ) to result.
    endloop.
    sort result by pgmid ascending object ascending obj_name ascending.
    delete adjacent duplicates from result comparing pgmid object obj_name.
  endmethod.

  method zif_ave_object~check_exists.
    select single devclass from tdevc where devclass = @me->id into @data(lv_d).
    result = cond #( when sy-subrc = 0 then abap_true else abap_false ).
  endmethod.

  method zif_ave_object~get_name.
    result = id.
  endmethod.

  method zif_ave_object~get_parts.
    loop at get_object_keys( ) into data(key).
      if key-pgmid = 'R3TR' and ( key-object = 'CLAS' or key-object = 'INTF' ).
        append value #(
          unit        = conv string( key-obj_name )
          object_name = conv versobjnam( key-obj_name )
          type        = conv versobjtyp( key-object ) ) to result.
      else.
        data(obj) = get_object( key ).
        if obj is bound.
          append lines of obj->get_parts( ) to result.
        else.
          append value #(
            unit        = conv string( key-obj_name )
            object_name = conv versobjnam( key-obj_name )
            type        = conv versobjtyp( key-object ) ) to result.
        endif.
      endif.
    endloop.
  endmethod.

endclass.

class zcl_ave_object_intf implementation.

  method constructor.
    me->name = name.
  endmethod.

  method zif_ave_object~check_exists.
    select single @abap_true into @result
      from seoclass
      where clsname = @name
        and clstype = '1'.
  endmethod.

  method zif_ave_object~get_name.
    result = name.
  endmethod.

  method zif_ave_object~get_parts.
    " Interface source is stored in a generated include; versions are
    " accessible via SVRS with objtype = 'REPS'.
    data lv_incname type program.
    try.
        lv_incname = cl_oo_classname_service=>get_intfsec_name( name ).
      catch cx_root.
        lv_incname = name.
    endtry.

    result = value #( (
      unit        = conv #( name )
      object_name = conv #( lv_incname )
      type        = 'REPS' ) ).
  endmethod.

endclass.

class zcl_ave_object_func implementation.

  method constructor.
    me->name = name.
  endmethod.

  method zif_ave_object~check_exists.
    call function 'FUNCTION_EXISTS'
      exporting
        funcname           = name
      exceptions
        function_not_exist = 1
        others             = 2.
    result = boolc( sy-subrc = 0 ).
  endmethod.

  method zif_ave_object~get_name.
    result = name.
  endmethod.

  method zif_ave_object~get_parts.
    result = value #( (
      unit        = conv #( name )
      object_name = conv #( name )
      type        = 'FUNC' ) ).
  endmethod.

endclass.

class zcl_ave_object_factory implementation.

  method get_instance.
    result = switch #(
      object_type
      when gc_type-program  then new zcl_ave_object_prog( object_name )
      when gc_type-class    then new zcl_ave_object_clas( conv #( object_name ) )
      when gc_type-intf     then new zcl_ave_object_intf( conv #( object_name ) )
      when gc_type-function then new zcl_ave_object_func( conv #( object_name ) )
      when gc_type-tr       then new zcl_ave_object_tr(   conv #( object_name ) )
      when gc_type-package  then new zcl_ave_object_pack( conv #( object_name ) )
      when gc_type-ddls     then new zcl_ave_object_ddls( conv #( object_name ) ) ).

    if result is not bound or result->check_exists( ) = abap_false.
      raise exception type zcx_ave.
    endif.
  endmethod.

endclass.

class zcl_ave_object_ddls implementation.

  method constructor.
    me->name = name.
  endmethod.

  method zif_ave_object~check_exists.
    data lv_name type tadir-obj_name.
    lv_name = name.
    select single pgmid from tadir
      where pgmid    = 'R3TR'
        and object   = 'DDLS'
        and obj_name = @lv_name
        and delflag  = ' '
      into @data(lv_pgmid).
    result = boolc( sy-subrc = 0 ).
  endmethod.

  method zif_ave_object~get_name.
    result = name.
  endmethod.

  method zif_ave_object~get_parts.
    result = value #( (
      unit        = conv #( name )
      object_name = name
      type        = 'DDLS' ) ).
  endmethod.

endclass.

class zcl_ave_object_clas implementation.
  method constructor.
    me->name = name.
  endmethod.
  method zif_ave_object~check_exists.
    cl_abap_classdescr=>describe_by_name(
      exporting
        p_name         = name
      exceptions
        type_not_found = 1
        others         = 2 ).
    result = boolc( sy-subrc = 0 ).
  endmethod.
  method zif_ave_object~get_name.
    result = name.
  endmethod.
  method zif_ave_object~get_parts.
    " Fixed sections of the class
    result = value #(
      ( class = name unit = 'Class pool'                 object_name = conv #( name )                                  type = 'CLSD' )
      ( class = name unit = 'Public section'             object_name = conv #( name )                                  type = 'CPUB' )
      ( class = name unit = 'Protected section'          object_name = conv #( name )                                  type = 'CPRO' )
      ( class = name unit = 'Private section'            object_name = conv #( name )                                  type = 'CPRI' )
      ( class = name unit = 'Local class definition'     object_name = conv #( cl_oo_classname_service=>get_ccdef_name( name ) ) type = 'CDEF' )
      ( class = name unit = 'Local class implementation' object_name = conv #( cl_oo_classname_service=>get_ccimp_name( name ) ) type = 'CINC' )
      ( class = name unit = 'Local macros'               object_name = conv #( cl_oo_classname_service=>get_ccmac_name( name ) ) type = 'CINC' )
      ( class = name unit = 'Local types'                object_name = conv #( cl_oo_classname_service=>get_cl_name( name ) )    type = 'REPS' )
      ( class = name unit = 'Test classes'               object_name = conv #( cl_oo_classname_service=>get_ccau_name( name ) )  type = 'CINC' ) ).

    " One entry per method
    call method cl_oo_classname_service=>get_all_method_includes
      exporting
        clsname            = name
      receiving
        result             = data(lt_meth)
      exceptions
        class_not_existing = 1.

    check sy-subrc = 0.

    " Загружаем все VRSD-записи для методов этого класса одним запросом
    data lv_like type versobjnam.
    lv_like = name.
    lv_like+30 = '%'.
    data lt_vrsd_meth type standard table of vrsd with empty key.
    select objname from vrsd
      where objtype = 'METH'
        and objname like @lv_like
      into table @lt_vrsd_meth.

    loop at lt_meth into data(method_include).
      data lv_objname type versobjnam.
      " Ищем точное имя из VRSD — SAP сам формирует ключ с правильным паддингом
      loop at lt_vrsd_meth into data(ls_vrsd)
        where objname+30 = method_include-cpdkey-cpdname.
        lv_objname = ls_vrsd-objname.
        exit.
      endloop.
      if lv_objname is initial.
        " Fallback: паддинг вручную через CHAR-присваивание
        lv_objname = name.
        lv_objname+30 = method_include-cpdkey-cpdname.
      endif.
      append value #(
        class       = name
        unit        = |{ method_include-cpdkey-cpdname }|
        object_name = lv_objname
        type        = 'METH'
      ) to result.
      clear lv_objname.
    endloop.
  endmethod.
endclass.

class zcl_ave_author implementation.

  method get_name.
    data author like line of authors.

    read table authors into author with key uname = uname.
    if sy-subrc <> 0.
      author-uname = uname.
      select name_textc into author-name
        up to 1 rows
        from user_addr
        where bname = uname
        order by name_textc.
        exit.
      endselect.
      if sy-subrc <> 0.
        author-name = uname.
      endif.
      insert author into table authors.
    endif.
    result = author-name.
  endmethod.

endclass.

class zcl_ave_acr_stats implementation.

  method is_blank_hunk.
    result = abap_true.
    loop at it_lines into data(lv_line).
      data(lv_trimmed) = condense( lv_line ).
      if lv_trimmed <> ''.
        result = abap_false.
        return.
      endif.
    endloop.
  endmethod.

  method from_diff.
    clear ev_ins. clear ev_del. clear ev_mod. clear et_authors.

    data lt_dels type string_table.
    data lt_ins  type string_table.

    " Append sentinel '=' to flush the last change block
    data lt_ops type zif_ave_popup_types=>ty_t_diff.
    lt_ops = it_diff.
    append value #( op = '=' ) to lt_ops.

    loop at lt_ops into data(ls).
      case ls-op.
        when '-'.
          append conv string( ls-text ) to lt_dels.
        when '+'.
          append conv string( ls-text ) to lt_ins.
        when '='.
          check lt_dels is not initial or lt_ins is not initial.

          " Skip hunks that contain only blank/whitespace lines — nothing to approve
          data lt_hunk_lines type string_table.
          clear lt_hunk_lines.
          loop at lt_dels into data(lv_dl). append lv_dl to lt_hunk_lines. endloop.
          loop at lt_ins  into data(lv_il). append lv_il to lt_hunk_lines. endloop.
          if is_blank_hunk( lt_hunk_lines ) = abap_true.
            clear lt_dels. clear lt_ins.
            continue.
          endif.

          " Parallel flag table: which ins lines have been matched already
          data lt_ins_matched type standard table of abap_bool with default key.
          clear lt_ins_matched.
          do lines( lt_ins ) times.
            append abap_false to lt_ins_matched.
          enddo.

          " First blamed line of the hunk claims the hunk_count for its author
          data lv_hunk_author type versuser.
          clear lv_hunk_author.

          " Greedy pairing: for each del, find first unmatched ins with has_common_chars
          loop at lt_dels into data(lv_d).
            data lv_paired type abap_bool.
            lv_paired = abap_false.
            loop at lt_ins into data(lv_i).
              data(lv_ii) = sy-tabix.
              assign lt_ins_matched[ lv_ii ] to field-symbol(<m>).
              check <m> = abap_false.
              if zcl_ave_popup_diff=>has_common_chars( iv_a = lv_d iv_b = lv_i ) = abap_true.
                ev_mod += 1.
                <m> = abap_true.
                lv_paired = abap_true.
                if it_blame is supplied.
                  data(lv_first_mod) = cond abap_bool(
                    when lv_hunk_author is initial then abap_true else abap_false ).
                  add_blame( exporting iv_text     = lv_i
                                       iv_op       = '~'
                                       iv_new_hunk = lv_first_mod
                                       it_blame    = it_blame
                             changing  ct_authors  = et_authors ).
                  if lv_first_mod = abap_true.
                    read table it_blame into data(ls_bm) with key text = lv_i.
                    if sy-subrc = 0. lv_hunk_author = ls_bm-author. endif.
                  endif.
                endif.
                exit.
              endif.
            endloop.
            if lv_paired = abap_false.
              ev_del += 1.
            endif.
          endloop.

          " Unmatched ins lines
          loop at lt_ins into lv_i.
            lv_ii = sy-tabix.
            assign lt_ins_matched[ lv_ii ] to <m>.
            check <m> = abap_false.
            ev_ins += 1.
            if it_blame is supplied.
              data(lv_first_ins) = cond abap_bool(
                when lv_hunk_author is initial then abap_true else abap_false ).
              add_blame( exporting iv_text     = lv_i
                                   iv_op       = '+'
                                   iv_new_hunk = lv_first_ins
                                   it_blame    = it_blame
                         changing  ct_authors  = et_authors ).
              if lv_first_ins = abap_true.
                read table it_blame into data(ls_bi) with key text = lv_i.
                if sy-subrc = 0. lv_hunk_author = ls_bi-author. endif.
              endif.
            endif.
          endloop.

          clear lt_dels. clear lt_ins. clear lt_ins_matched. clear lv_hunk_author.
      endcase.
    endloop.
  endmethod.

  method add_blame.
    read table it_blame into data(ls_b) with key text = iv_text.
    check sy-subrc = 0.
    read table ct_authors assigning field-symbol(<a>) with key author = ls_b-author.
    if sy-subrc <> 0.
      insert value #( author = ls_b-author author_name = ls_b-author_name )
        into table ct_authors.
      read table ct_authors assigning <a> with key author = ls_b-author.
    endif.
    case iv_op.
      when '+'. <a>-ins_count += 1.
      when '~'. <a>-mod_count += 1.
    endcase.
    if iv_new_hunk = abap_true.
      <a>-hunk_count += 1.
    endif.
  endmethod.

endclass.

class zcl_ave_acr_report implementation.
  method to_html.
    " Transport description from E07T
    data lv_korr_text type as4text.
    select single as4text from e07t
      where trkorr = @i_korrnum and langu = @sy-langu
      into @lv_korr_text.

    " Aggregate grand totals per owner across all objects
    types: begin of ty_owner_total,
             author      type versuser,
             author_name type ad_namtext,
             ins_count   type i,
             mod_count   type i,
             del_count   type i,
             hunk_count  type i,
             appr_count  type i,
             decl_count  type i,
           end of ty_owner_total.
    data lt_totals type standard table of ty_owner_total with default key.

    loop at it_obj_stats into data(ls_obj).
      " Compute approved/declined for this object
      data(lv_obj_prefix) = |{ ls_obj-objtype }~{ ls_obj-obj_name }~|.
      data(lv_cp_pat2) = lv_obj_prefix && `*`.
      data lv_oa type i.
      data lv_od type i.
      clear: lv_oa, lv_od.
      loop at it_approved into data(lv_ak2). if lv_ak2 cp lv_cp_pat2. lv_oa += 1. endif. endloop.
      loop at it_declined into data(lv_dk2). if lv_dk2 cp lv_cp_pat2. lv_od += 1. endif. endloop.
      if lv_oa > ls_obj-hunk_count.
        lv_oa = ls_obj-hunk_count.
      endif.
      if lv_od > ls_obj-hunk_count.
        lv_od = ls_obj-hunk_count.
      endif.

      if ls_obj-bt_authors is not initial.

        loop at ls_obj-bt_authors into data(ls_ba).
          read table lt_totals assigning field-symbol(<t>) with key author = ls_ba-author.
          if sy-subrc <> 0.
            append value #( author = ls_ba-author author_name = ls_ba-author_name ) to lt_totals.
            read table lt_totals assigning <t> with key author = ls_ba-author.
          endif.
          <t>-ins_count  += ls_ba-ins_count.
          <t>-del_count  += ls_ba-del_count.
          <t>-mod_count  += ls_ba-mod_count.
          <t>-hunk_count += ls_ba-hunk_count.
        endloop.

        " approved/declined go to primary author (most ins, then mod lines)
        data lv_primary      type versuser.
        data lv_primary_ins  type i.
        data lv_primary_mod  type i.
        clear: lv_primary, lv_primary_ins, lv_primary_mod.
        loop at ls_obj-bt_authors into ls_ba.
          if ls_ba-ins_count > lv_primary_ins.
            lv_primary_ins = ls_ba-ins_count.
            lv_primary_mod = ls_ba-mod_count.
            lv_primary     = ls_ba-author.
          elseif ls_ba-ins_count = lv_primary_ins and ls_ba-mod_count > lv_primary_mod.
            lv_primary_mod = ls_ba-mod_count.
            lv_primary     = ls_ba-author.
          endif.
        endloop.
        if lv_primary is initial.
          data lv_primary_del type i.
          clear lv_primary_del.
          loop at ls_obj-bt_authors into ls_ba.
            if ls_ba-del_count > lv_primary_del.
              lv_primary_del = ls_ba-del_count.
              lv_primary     = ls_ba-author.
            endif.
          endloop.
        endif.
        if lv_primary is not initial.
          read table lt_totals assigning <t> with key author = lv_primary.
          if sy-subrc = 0.
            <t>-appr_count += lv_oa.
            <t>-decl_count += lv_od.
          endif.
        endif.
      elseif ls_obj-author is not initial.
        read table lt_totals assigning <t> with key author = ls_obj-author.
        if sy-subrc <> 0.
          append value #( author = ls_obj-author author_name = ls_obj-author_name ) to lt_totals.
          read table lt_totals assigning <t> with key author = ls_obj-author.
        endif.
        <t>-ins_count  += ls_obj-ins_count.
        <t>-del_count  += ls_obj-del_count.
        <t>-mod_count  += ls_obj-mod_count.
        <t>-hunk_count += ls_obj-hunk_count.
        <t>-appr_count += lv_oa.
        <t>-decl_count += lv_od.
      endif.
    endloop.

    " Shared CSS (matches AVE's Consolas/monospace style)
    data(lv_css) =
      `body{font:13px/1.6 Consolas,monospace;padding:20px 28px;background:#fff;color:#333}` &&
      `h2{color:#2c3e50;border-bottom:2px solid #3498db;padding-bottom:6px;margin-bottom:16px}` &&
      `h3{color:#555;margin:20px 0 6px}` &&
      `table{border-collapse:collapse;width:100%;margin-bottom:16px;font-size:12px}` &&
      `th{background:#3498db;color:#fff;padding:5px 10px;text-align:left;white-space:nowrap}` &&
      `td{padding:4px 10px;border-bottom:1px solid #eee;white-space:nowrap}` &&
      `tr:hover td{background:#f5f9ff}` &&
      `td:nth-child(2){width:220px;min-width:220px;max-width:220px;overflow:hidden;text-overflow:ellipsis}` &&
      `tr.obj-row{cursor:pointer}` &&
      `tr.obj-row:hover td{background:#e8f0fb}` &&
      `tr.user-row{cursor:pointer}` &&
      `tr.user-row:hover td{background:#e8f0fb}` &&
      `.cr td{background:#f0f4f8;font-weight:bold}` &&
      `.mr td:nth-child(3){padding-left:24px}` &&
      `.nr{text-align:right}` &&
      `.gi{color:#27ae60}.gd{color:#e74c3c}.gm{color:#e67e22}`.

    result =
      |<!DOCTYPE html><html><head><meta charset="utf-8">| &&
      |<style>{ lv_css }</style>| &&
      `<script>x=1;</script></head><body>`.

    " ── Header ──────────────────────────────────────────────────────
    result = result &&
      |<h2>&#128196;&nbsp;Code Review Report&nbsp;&mdash;&nbsp;| &&
      |<span style="color:#3498db">{ esc( i_korrnum ) }|.
    if lv_korr_text is not initial.
      result = result && |&nbsp;&mdash;&nbsp;{ esc( lv_korr_text ) }|.
    endif.
    result = result && |</span></h2>|.

    " ── Authors table ───────────────────────────────────────────────
    if lt_totals is not initial.
      result = result &&
        |<h3>Developers</h3>| &&
        |<table><tr>| &&
        |<th>Developer</th><th>Name</th>| &&
        |<th class="nr">Ins/Mod/Del</th>| &&
        |<th class="nr">Blocks</th>| &&
        |<th class="nr">Approved</th>| &&
        |<th class="nr">Declined</th>| &&
        |<th class="nr">%</th></tr>|.
      loop at lt_totals into data(ls_tot).
        check ls_tot-ins_count > 0 or ls_tot-mod_count > 0 or ls_tot-del_count > 0
           or ls_tot-hunk_count > 0.
        " Build approved/declined/% cells for owner row
        data lv_ow_appr_cell type string.
        data lv_ow_decl_cell type string.
        data lv_ow_pct_cell  type string.
        data lv_ow_pct       type i.
        if ls_tot-hunk_count = 0.
          lv_ow_appr_cell = `<td class="nr">—</td>`.
          lv_ow_decl_cell = `<td class="nr">—</td>`.
          lv_ow_pct_cell  = `<td class="nr">—</td>`.
        else.
          data(lv_ow_done) = ls_tot-appr_count + ls_tot-decl_count.
          if lv_ow_done > ls_tot-hunk_count.
            lv_ow_done = ls_tot-hunk_count.
          endif.
          lv_ow_pct = lv_ow_done * 100 / ls_tot-hunk_count.
          " Approved: green only at 100% approved
          if ls_tot-appr_count = ls_tot-hunk_count.
            lv_ow_appr_cell = |<td class="nr gi" style="font-weight:bold">&#10003; { ls_tot-appr_count }/{ ls_tot-hunk_count }</td>|.
          elseif ls_tot-appr_count > 0.
            lv_ow_appr_cell = |<td class="nr" style="font-weight:bold">&#10003; { ls_tot-appr_count }/{ ls_tot-hunk_count }</td>|.
          else.
            lv_ow_appr_cell = |<td class="nr">{ ls_tot-appr_count }/{ ls_tot-hunk_count }</td>|.
          endif.
          " Declined: red only at 100% declined
          if ls_tot-decl_count = ls_tot-hunk_count.
            lv_ow_decl_cell = |<td class="nr gd" style="font-weight:bold">&#10007; { ls_tot-decl_count }/{ ls_tot-hunk_count }</td>|.
          elseif ls_tot-decl_count > 0.
            lv_ow_decl_cell = |<td class="nr" style="font-weight:bold">&#10007; { ls_tot-decl_count }/{ ls_tot-hunk_count }</td>|.
          else.
            lv_ow_decl_cell = |<td class="nr">{ ls_tot-decl_count }/{ ls_tot-hunk_count }</td>|.
          endif.
          " %: green at 100% approved, red at 100% declined
          if ls_tot-appr_count = ls_tot-hunk_count.
            lv_ow_pct_cell = |<td class="nr gi" style="font-weight:bold">{ lv_ow_pct }%</td>|.
          elseif ls_tot-decl_count = ls_tot-hunk_count.
            lv_ow_pct_cell = |<td class="nr gd" style="font-weight:bold">{ lv_ow_pct }%</td>|.
          else.
            lv_ow_pct_cell = |<td class="nr" style="font-weight:bold">{ lv_ow_pct }%</td>|.
          endif.
        endif.
        data(lv_user_tr_attr) = `class="user-row" title="Click to show declined notes"`.
        result = result &&
          |<tr { lv_user_tr_attr }>| &&
          |<td style="font-weight:bold"><a href="sapevent:openuserdeclined~{ esc( ls_tot-author ) }">{ esc( ls_tot-author ) }</a></td>| &&
          |<td style="font-weight:bold">{ esc( ls_tot-author_name ) }</td>| &&
          |<td class="nr" style="font-weight:bold">| &&
            |<span style="color:#27ae60">{ ls_tot-ins_count }</span>| &&
            |&nbsp;/&nbsp;<span style="color:#e67e22">{ ls_tot-mod_count }</span>| &&
            |&nbsp;/&nbsp;<span style="color:#e74c3c">{ ls_tot-del_count }</span>| &&
          |</td>| &&
          |<td class="nr" style="font-weight:bold">{ ls_tot-hunk_count }</td>| &&
          lv_ow_appr_cell && lv_ow_decl_cell && lv_ow_pct_cell &&
          |</tr>|.
      endloop.
      result = result && |</table>|.
    endif.

    " ── Changed objects table ────────────────────────────────────────
    if it_reviewers is not initial.
      result = result &&
        |<h3>Reviewers</h3>| &&
        |<table><tr>| &&
        |<th>Reviewer</th><th>Name</th>| &&
        |<th class="nr">Approved</th>| &&
        |<th class="nr">Declined</th>| &&
        |<th class="nr">Total</th></tr>|.
      loop at it_reviewers into data(ls_rev).
        check ls_rev-total_count > 0.
        result = result &&
          |<tr>| &&
          |<td style="font-weight:bold"><a href="sapevent:openreviewer~{ esc( ls_rev-reviewer ) }">{ esc( ls_rev-reviewer ) }</a></td>| &&
          |<td style="font-weight:bold">{ esc( ls_rev-reviewer_name ) }</td>| &&
          |<td class="nr gi" style="font-weight:bold">{ ls_rev-appr_count }</td>| &&
          |<td class="nr gd" style="font-weight:bold">{ ls_rev-decl_count }</td>| &&
          |<td class="nr" style="font-weight:bold">{ ls_rev-total_count }</td>| &&
          |</tr>|.
      endloop.
      result = result && |</table>|.
    endif.

    types: begin of ty_sort,
             class_name type seoclsname,
             type_order type i,
             obj_name   type versobjnam,
             idx        type i,
           end of ty_sort.
    data lt_sort type standard table of ty_sort with default key.
    data lt_sorted type zif_ave_acr_types=>ty_t_obj_stats.
    lt_sorted = it_obj_stats.

    loop at lt_sorted into data(ls_s2).
      data(lv_ord) = switch i( ls_s2-objtype
        when 'CLSD' then 1
        when 'CPUB' then 2
        when 'CPRO' then 3
        when 'CPRI' then 4
        when 'CINC' then 5
        when 'CDEF' then 6
        when 'METH' then 7
        else             0 ).
      data(lv_class_name) = ls_s2-class_name.
      if lv_class_name is initial.
        case ls_s2-objtype.
          when 'CLSD' or 'CPUB' or 'CPRO' or 'CPRI' or 'CINC' or 'CDEF'.
            data(lv_obj_name) = conv string( ls_s2-obj_name ).
            find first occurrence of '=' in lv_obj_name match offset data(lv_eq_pos).
            if sy-subrc = 0.
              lv_obj_name = lv_obj_name(lv_eq_pos).
            endif.
            lv_class_name = conv #( lv_obj_name ).
        endcase.
      endif.
      append value #( class_name = lv_class_name
                      type_order = lv_ord
                      obj_name   = ls_s2-obj_name
                      idx        = sy-tabix ) to lt_sort.
    endloop.
    sort lt_sort by class_name type_order obj_name.

    data lt_sorted_final type zif_ave_acr_types=>ty_t_obj_stats.
    loop at lt_sort into data(ls_ord).
      read table lt_sorted into data(ls_tmp) index ls_ord-idx.
      if ls_tmp-class_name is initial.
        ls_tmp-class_name = ls_ord-class_name.
      endif.
      append ls_tmp to lt_sorted_final.
    endloop.

    " Remove entries with no actual changes
    delete lt_sorted_final where ins_count = 0 and del_count = 0 and mod_count = 0.

    " Render one table per class (empty class_name = programs/other)
    data lv_cur_class type seoclsname value '####'.
    data(lv_tbl_hdr) =
      |<table><tr>| &&
      |<th>Type</th><th>Object</th>| &&
      |<th>Owner</th><th>Date</th><th>Time</th>| &&
      |<th class="nr">Ins/Mod/Del</th>| &&
      |<th class="nr">Blocks</th>| &&
      |<th class="nr">Approved</th>| &&
      |<th class="nr">Declined</th>| &&
      |<th class="nr">%</th></tr>|.

    " Class-level totals accumulators
    data lv_tot_ins     type i.
    data lv_tot_mod     type i.
    data lv_tot_del     type i.
    data lv_tot_hunks   type i.
    data lv_tot_appr    type i.
    data lv_tot_decl    type i.

    data lv_tot_pct       type i.
    data lv_tot_appr_cell type string.
    data lv_tot_decl_cell type string.
    data lv_tot_pct_cell  type string.

    loop at lt_sorted_final into ls_obj.
      if ls_obj-class_name <> lv_cur_class.
        " ── close previous table with Total row ──
        if lv_cur_class <> '####'.
          if lv_tot_hunks = 0.
            lv_tot_appr_cell = `<td class="nr">—</td>`.
            lv_tot_decl_cell = `<td class="nr">—</td>`.
            lv_tot_pct_cell  = `<td class="nr">—</td>`.
          else.
            data(lv_class_done) = lv_tot_appr + lv_tot_decl.
            if lv_class_done > lv_tot_hunks.
              lv_class_done = lv_tot_hunks.
            endif.
            lv_tot_pct = lv_class_done * 100 / lv_tot_hunks.
            if lv_tot_appr > 0.
              lv_tot_appr_cell = |<td class="nr gi" style="font-weight:bold">&#10003; { lv_tot_appr }/{ lv_tot_hunks }</td>|.
            else.
              lv_tot_appr_cell = |<td class="nr" style="font-weight:bold">{ lv_tot_appr }/{ lv_tot_hunks }</td>|.
            endif.
            if lv_tot_decl > 0.
              lv_tot_decl_cell = |<td class="nr gd" style="font-weight:bold">&#10007; { lv_tot_decl }/{ lv_tot_hunks }</td>|.
            else.
              lv_tot_decl_cell = |<td class="nr" style="font-weight:bold">{ lv_tot_decl }/{ lv_tot_hunks }</td>|.
            endif.
            lv_tot_pct_cell = |<td class="nr" style="font-weight:bold">{ lv_tot_pct }%</td>|.
          endif.
          result = result &&
            `<tr style="background:#e8f0fb;border-top:2px solid #3498db">` &&
            `<td style="font-weight:bold;color:#2c3e50" colspan="2">Total</td>` &&
            `<td colspan="3"></td>` &&
            |<td class="nr" style="font-weight:bold">| &&
              |<span style="color:#27ae60">{ lv_tot_ins }</span>| &&
              |&nbsp;/&nbsp;<span style="color:#e67e22">{ lv_tot_mod }</span>| &&
              |&nbsp;/&nbsp;<span style="color:#e74c3c">{ lv_tot_del }</span></td>| &&
            |<td class="nr" style="font-weight:bold">{ lv_tot_hunks }</td>| &&
            lv_tot_appr_cell && lv_tot_decl_cell && lv_tot_pct_cell &&
            `</tr></table>`.
          clear: lv_tot_ins, lv_tot_mod, lv_tot_del, lv_tot_hunks, lv_tot_appr, lv_tot_decl.
        endif.
        lv_cur_class = ls_obj-class_name.
        if lv_cur_class is initial.
          result = result && |<h3>Programs / Other</h3>|.
        else.
          result = result && |<h3>Class: { esc( lv_cur_class ) }</h3>|.
        endif.
        result = result && lv_tbl_hdr.
      endif.

      " Format date/time for display
      data(lv_date) = conv string( ls_obj-datum ).
      if lv_date is not initial.
        lv_date = |{ lv_date(4) }-{ lv_date+4(2) }-{ lv_date+6(2) }|.
      endif.
      data(lv_time) = conv string( ls_obj-zeit ).
      if lv_time is not initial.
        lv_time = |{ lv_time(2) }:{ lv_time+2(2) }:{ lv_time+4(2) }|.
      endif.

      " Compute approve/decline stats for this object
      lv_obj_prefix = |{ ls_obj-objtype }~{ ls_obj-obj_name }~|.
      data(lv_cp_pat) = lv_obj_prefix && `*`.
      data lv_appr type i.
      data lv_decl type i.
      clear: lv_appr, lv_decl.
      loop at it_approved into data(lv_ak).
        if lv_ak cp lv_cp_pat. lv_appr += 1. endif.
      endloop.
      loop at it_declined into data(lv_dk).
        if lv_dk cp lv_cp_pat. lv_decl += 1. endif.
      endloop.
      data lv_total_h      type i.
      data lv_approve_cell type string.
      data lv_decline_cell type string.
      data lv_pct_cell     type string.
      data lv_pct          type i.
      clear: lv_total_h, lv_approve_cell, lv_decline_cell, lv_pct_cell, lv_pct.
      lv_total_h = ls_obj-hunk_count.
      if lv_appr > lv_total_h.
        lv_appr = lv_total_h.
      endif.
      if lv_decl > lv_total_h.
        lv_decl = lv_total_h.
      endif.
      if lv_total_h = 0.
        lv_approve_cell = `<td class="nr">—</td>`.
        lv_decline_cell = `<td class="nr">—</td>`.
        lv_pct_cell     = `<td class="nr">—</td>`.
      else.
        data(lv_obj_done) = lv_appr + lv_decl.
        if lv_obj_done > lv_total_h.
          lv_obj_done = lv_total_h.
        endif.
        lv_pct = lv_obj_done * 100 / lv_total_h.
        " Approved: green only at 100% approved
        if lv_appr = lv_total_h.
          lv_approve_cell = |<td class="nr gi" style="font-weight:bold">&#10003; { lv_appr }/{ lv_total_h }</td>|.
        elseif lv_appr > 0.
          lv_approve_cell = |<td class="nr" style="font-weight:bold">&#10003; { lv_appr }/{ lv_total_h }</td>|.
        else.
          lv_approve_cell = |<td class="nr">{ lv_appr }/{ lv_total_h }</td>|.
        endif.
        " Declined: red only at 100% declined
        if lv_decl = lv_total_h.
          lv_decline_cell = |<td class="nr gd" style="font-weight:bold">&#10007; { lv_decl }/{ lv_total_h }</td>|.
        elseif lv_decl > 0.
          lv_decline_cell = |<td class="nr" style="font-weight:bold">&#10007; { lv_decl }/{ lv_total_h }</td>|.
        else.
          lv_decline_cell = |<td class="nr">{ lv_decl }/{ lv_total_h }</td>|.
        endif.
        " %: green at 100% approved, red at 100% declined
        if lv_appr = lv_total_h.
          lv_pct_cell = |<td class="nr gi" style="font-weight:bold">{ lv_pct }%</td>|.
        elseif lv_decl = lv_total_h.
          lv_pct_cell = |<td class="nr gd" style="font-weight:bold">{ lv_pct }%</td>|.
        else.
          lv_pct_cell = |<td class="nr" style="font-weight:bold">{ lv_pct }%</td>|.
        endif.
      endif.

      " Accumulate class totals
      lv_tot_ins     += ls_obj-ins_count.
      lv_tot_mod     += ls_obj-mod_count.
      lv_tot_del     += ls_obj-del_count.
      lv_tot_hunks   += ls_obj-hunk_count.
      lv_tot_appr    += lv_appr.
      lv_tot_decl    += lv_decl.

      data(lv_ev_key) = |{ ls_obj-objtype }~{ ls_obj-obj_name }|.
      data lv_disp_name type string.
      lv_disp_name = cond #( when ls_obj-display_name is not initial then ls_obj-display_name else ls_obj-obj_name ).
      data(lv_row_id) = |obj_{ escape( val = lv_ev_key format = cl_abap_format=>e_html_attr ) }|.
      data lv_name_cell type string.
      if ls_obj-is_created = abap_true.
        lv_name_cell = |<td><a href="sapevent:openobj~{ lv_ev_key }" style="font-weight:bold;color:#27ae60">{ esc( lv_disp_name ) }</a></td>|.
      else.
        lv_name_cell = |<td><a href="sapevent:openobj~{ lv_ev_key }" style="font-weight:bold">{ esc( lv_disp_name ) }</a></td>|.
      endif.
      data lv_owner_display type string.
      data lv_owner_count type i.
      clear: lv_owner_display, lv_owner_count.
      if ls_obj-bt_authors is not initial.
        loop at ls_obj-bt_authors into data(ls_owner_ba) where hunk_count > 0.
          check ls_owner_ba-author is not initial.
          lv_owner_count += 1.
          if lv_owner_count <= 3.
            if lv_owner_display is initial.
              lv_owner_display = ls_owner_ba-author.
            else.
              lv_owner_display = lv_owner_display && `, ` && ls_owner_ba-author.
            endif.
          endif.
        endloop.
        if lv_owner_count > 3.
          lv_owner_display = `Several`.
        endif.
      endif.
      if lv_owner_display is initial.
        lv_owner_display = ls_obj-author.
      endif.
      result = result &&
        |<tr id="{ lv_row_id }">| &&
        |<td>{ esc( ls_obj-objtype ) }</td>| &&
        lv_name_cell &&
        |<td>{ esc( lv_owner_display ) }</td>| &&
        |<td>{ lv_date }</td>| &&
        |<td>{ lv_time }</td>| &&
        |<td class="nr" style="font-weight:bold">| &&
          |<span style="color:#27ae60">{ ls_obj-ins_count }</span>| &&
          |&nbsp;/&nbsp;<span style="color:#e67e22">{ ls_obj-mod_count }</span>| &&
          |&nbsp;/&nbsp;<span style="color:#e74c3c">{ ls_obj-del_count }</span></td>| &&
        |<td class="nr" style="font-weight:bold">{ ls_obj-hunk_count }</td>| &&
        lv_approve_cell && lv_decline_cell && lv_pct_cell &&
        `</tr>`.
    endloop.

    " ── close last table with Total row ──
    if lv_cur_class <> '####'.
      if lv_tot_hunks = 0.
        lv_tot_appr_cell = `<td class="nr">—</td>`.
        lv_tot_decl_cell = `<td class="nr">—</td>`.
        lv_tot_pct_cell  = `<td class="nr">—</td>`.
      else.
        data(lv_group_done) = lv_tot_appr + lv_tot_decl.
        if lv_group_done > lv_tot_hunks.
          lv_group_done = lv_tot_hunks.
        endif.
        lv_tot_pct = lv_group_done * 100 / lv_tot_hunks.
        if lv_tot_appr > 0.
          lv_tot_appr_cell = |<td class="nr gi" style="font-weight:bold">&#10003; { lv_tot_appr }/{ lv_tot_hunks }</td>|.
        else.
          lv_tot_appr_cell = |<td class="nr" style="font-weight:bold">{ lv_tot_appr }/{ lv_tot_hunks }</td>|.
        endif.
        if lv_tot_decl > 0.
          lv_tot_decl_cell = |<td class="nr gd" style="font-weight:bold">&#10007; { lv_tot_decl }/{ lv_tot_hunks }</td>|.
        else.
          lv_tot_decl_cell = |<td class="nr" style="font-weight:bold">{ lv_tot_decl }/{ lv_tot_hunks }</td>|.
        endif.
        lv_tot_pct_cell = |<td class="nr" style="font-weight:bold">{ lv_tot_pct }%</td>|.
      endif.
      result = result &&
        `<tr style="background:#e8f0fb;border-top:2px solid #3498db">` &&
        `<td style="font-weight:bold;color:#2c3e50" colspan="2">Total</td>` &&
        `<td colspan="3"></td>` &&
        |<td class="nr" style="font-weight:bold">| &&
          |<span style="color:#27ae60">{ lv_tot_ins }</span>| &&
          |&nbsp;/&nbsp;<span style="color:#e67e22">{ lv_tot_mod }</span>| &&
          |&nbsp;/&nbsp;<span style="color:#e74c3c">{ lv_tot_del }</span></td>| &&
        |<td class="nr" style="font-weight:bold">{ lv_tot_hunks }</td>| &&
        lv_tot_appr_cell && lv_tot_decl_cell && lv_tot_pct_cell &&
        `</tr></table>`.
    endif.

    result = result && |</body></html>|.
  endmethod.
  method esc.
    result = escape( val = conv string( iv_val ) format = cl_abap_format=>e_html_text ).
  endmethod.
endclass.

class zcl_ave_acr_note_dlg implementation.

  method constructor.
    mv_title    = iv_title.
    mv_hunk_key = iv_hunk_key.
    mv_note     = iv_note.
  endmethod.
  method show.
    " ── Dialog box ──────────────────────────────────────────────────
    create object mo_box
      exporting
        width                       = 560
        height                      = 160
        top                         = 120
        left                        = 200
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
    if sy-subrc <> 0. return. endif.

    mo_box->set_caption( conv text40( mv_title ) ).
    set handler on_box_close for mo_box.

    " ── Text editor fills the whole dialog ──────────────────────────
    create object mo_text
      exporting
        parent                 = mo_box
        wordwrap_mode          = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position      = 255
      exceptions
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        others                 = 6.
    if sy-subrc <> 0. return. endif.

    " Pre-fill existing note if editing
    if mv_note is not initial.
      data lt_lines type table of char255.
      data lv_rest  type string.
      lv_rest = mv_note.
      while strlen( lv_rest ) > 255.
        append conv char255( lv_rest(255) ) to lt_lines.
        lv_rest = lv_rest+255.
      endwhile.
      append conv char255( lv_rest ) to lt_lines.
      mo_text->set_text_as_r3table( lt_lines ).
    endif.

    cl_gui_control=>set_focus( control = mo_text ).
    cl_gui_cfw=>flush( ).
  endmethod.
  method on_box_close.
    " Read text — if not empty, register decline with note.
    " Do NOT call free() here — the framework closes the container automatically.
    data lt_lines type table of char255.
    mo_text->get_text_as_r3table(
      importing
        table = lt_lines ).

    data lv_note type string.
    loop at lt_lines into data(lv_line).
      data(lv_str) = condense( conv string( lv_line ) ).
      if lv_str is not initial.
        if lv_note is initial.
          lv_note = lv_str.
        else.
          lv_note = lv_note && cl_abap_char_utilities=>newline && lv_str.
        endif.
      endif.
    endloop.

    sender->free( ).
    clear mo_box.

    if lv_note is not initial.
      raise event saved
        exporting iv_hunk_key = mv_hunk_key
                  iv_note     = lv_note.
    else.
      raise event cancelled
        exporting iv_hunk_key = mv_hunk_key.
    endif.
  endmethod.

endclass.

" & Multi-windows program for ABAP object version comparison
" &----------------------------------------------------------------------
" & version: 1.00, 0.5 for Code Reviewer
" & Git https://github.com/ysichov/AVE

" & Written by Yurii Sychov
" & e-mail:   ysichov@gmail.com
" & blog:     https://ysychov.wordpress.com/blog/
" & LinkedIn: https://www.linkedin.com/in/ysychov/

" &Inspired by https://github.com/abapinho/abapTimeMachine , Eclipse Adt, GitHub and all others similar tools
" &----------------------------------------------------------------------
data go_popup type ref to zcl_ave_popup.


selection-screen begin of block b_mode with frame title gv_txt01.
  parameters: p_cr radiobutton group mode  user-command umod default 'X'.
  parameters: p_ve radiobutton group mode .

selection-screen end of block b_mode.

selection-screen begin of block b1 with frame title gv_txt02.

  selection-screen begin of line.
    parameters rb_tr   radiobutton  group typ user-command utyp default 'X'.
    selection-screen comment 3(20) gv_txt11 for field rb_tr.
    parameters p_task  type trkorr                                     modif id trq.
  selection-screen end of line.
  selection-screen begin of line.
    parameters rb_prog radiobutton group typ .
    selection-screen comment 3(20) gv_txt12 for field rb_prog.
    parameters p_prog  type progname   matchcode object progname      modif id prg.
  selection-screen end of line.

  selection-screen begin of line.
    parameters rb_clas radiobutton group typ.
    selection-screen comment 3(20) gv_txt13 for field rb_clas.
    parameters p_clas  type seoclsname matchcode object sfbeclname    modif id cls.
  selection-screen end of line.

  selection-screen begin of line.
    parameters rb_func radiobutton group typ.
    selection-screen comment 3(20) gv_txt14 for field rb_func.
    parameters p_func  type rs38l_fnam matchcode object cacs_function modif id fnc.
  selection-screen end of line.
  selection-screen begin of line.
    parameters rb_pack radiobutton group typ.
    selection-screen comment 3(20) gv_txt15 for field rb_pack.
    parameters p_pack  type devclass   matchcode object devclass       modif id pck.
  selection-screen end of line.

  selection-screen begin of line.
    parameters rb_ddls radiobutton group typ.
    selection-screen comment 3(20) gv_txt16 for field rb_ddls.
    parameters p_ddls  type versobjnam                                  modif id dls.
  selection-screen end of line.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title gv_txt03.
  parameters p_layout as checkbox default 'X'.
  parameters p_pane as checkbox.
  parameters p_cmpct as checkbox default 'X'.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title gv_txt04.
  parameters p_diff no-display default 'X'.
  parameters p_datefr type versdate.
  parameters p_rmdp  as checkbox.
  parameters p_ntoc as checkbox default 'X'.
  parameters p_icase  as checkbox default 'X'.
selection-screen end of block b3.

selection-screen begin of block b4 with frame title gv_txt05.
  parameters p_blame as checkbox.
  parameters p_user type versuser.
selection-screen end of block b4.

"Events

initialization.
  p_user = sy-uname.
  perform supress_button.

  perform init_desc.



at selection-screen output.
  loop at screen.
    case screen-group1.
      when 'PRG'.
        screen-input = cond #( when rb_prog = 'X' then 1 else 0 ).
      when 'CLS'.
        screen-input = cond #( when rb_clas = 'X' then 1 else 0 ).
      when 'FNC'.
        screen-input = cond #( when rb_func = 'X' then 1 else 0 ).
      when 'TRQ'.
        screen-input = cond #( when rb_tr   = 'X' then 1 else 0 ).
      when 'PCK'.
        screen-input = cond #( when rb_pack = 'X' then 1 else 0 ).
      when 'DLS'.
        screen-input = cond #( when rb_ddls = 'X' then 1 else 0 ).
    endcase.
    if screen-name = 'P_PANE' or screen-name = 'P_CMPCT'.
      screen-input = cond #( when p_diff = 'X' then 1 else 0 ).
    endif.
    modify screen.
  endloop.

at selection-screen on p_diff.
  " Trigger OUTPUT to re-evaluate enabled state of dependent checkboxes

at selection-screen.
  check sy-ucomm <> 'DUMMY'.
  perform run_ave.

form supress_button.
  data itab type table of sy-ucomm.
  append 'ONLI' to itab.
  call function 'RS_SET_SELSCREEN_STATUS'
    exporting
      p_status  = sy-pfkey
    tables
      p_exclude = itab.
endform.

form init_desc .
  gv_txt01 = '应用类型'.
  gv_txt02 = '选择要操作的对象类型'.
  gv_txt03 = '布局 / 界面偏好设置'.
  gv_txt04 = '数据选项'.
  gv_txt05 = '高亮用户'.
  gv_txt11 = '传输请求 / 任务'.
  gv_txt12 = '程序 / 包含程序'.
  gv_txt13 = '类'.
  gv_txt14 = '函数模块'.
  gv_txt15 = '开发包'.
  gv_txt16 = 'CDS 视图'.

  %_p_cr_%_app_%-text = '代码评审器'.
  %_p_ve_%_app_%-text = '版本浏览器'.
  %_p_layout_%_app_%-text = '侧边栏布局/自上而下布局'.
  %_p_pane_%_app_%-text = '双窗格 / 单窗格'.
  %_p_cmpct_%_app_%-text = '精简视图 / 完整文本'.
  %_p_datefr_%_app_%-text = '起始日期'.
  %_p_rmdp_%_app_%-text = '移除相同版本'.
  %_p_ntoc_%_app_%-text = '不显示目录'.
  %_p_icase_%_app_%-text = '忽略大小写'.
  %_p_blame_%_app_%-text = '代码责任人追溯'.
  %_p_user_%_app_%-text = '变更对象高亮用户'.

endform.

form run_ave.
  " Open popup only when the user pressed Enter (ucomm is initial)
  check sy-ucomm is initial.

  try.
      data(ls_settings) = value zif_ave_object=>ty_settings(
        show_diff   = conv #( p_diff )
        layout      = conv #( p_layout )
        two_pane    = conv #( p_pane )
        no_toc      = conv #( p_ntoc )
        ignore_case = conv #( p_icase )
        compact     = conv #( p_cmpct )
        remove_dup  = conv #( p_rmdp )
        blame       = conv #( p_blame )
        filter_user = p_user
        date_from   = p_datefr
        code_review = conv #( p_cr ) ).

      if rb_prog = 'X' and p_prog is not initial.
        go_popup = new zcl_ave_popup(
          i_object_type = zcl_ave_object_factory=>gc_type-program
          i_object_name = conv #( p_prog )
          is_settings   = ls_settings ).

      elseif rb_clas = 'X' and p_clas is not initial.
        go_popup = new zcl_ave_popup(
          i_object_type = zcl_ave_object_factory=>gc_type-class
          i_object_name = conv #( p_clas )
          is_settings   = ls_settings ).

      elseif rb_func = 'X' and p_func is not initial.
        go_popup = new zcl_ave_popup(
          i_object_type = zcl_ave_object_factory=>gc_type-function
          i_object_name = conv #( p_func )
          is_settings   = ls_settings ).

      elseif rb_tr = 'X' and p_task is not initial.
        go_popup = new zcl_ave_popup(
          i_object_type = zcl_ave_object_factory=>gc_type-tr
          i_object_name = conv #( p_task )
          is_settings   = ls_settings ).

      elseif rb_pack = 'X' and p_pack is not initial.
        go_popup = new zcl_ave_popup(
          i_object_type = zcl_ave_object_factory=>gc_type-package
          i_object_name = conv #( p_pack )
          is_settings   = ls_settings ).

      elseif rb_ddls = 'X' and p_ddls is not initial.
        go_popup = new zcl_ave_popup(
          i_object_type = zcl_ave_object_factory=>gc_type-ddls
          i_object_name = conv #( p_ddls )
          is_settings   = ls_settings ).

      else.
        message 'Please enter an object name.' type 'W'.
        return.
      endif.

      go_popup->show( ).

    catch zcx_ave into data(lx).
      message lx->get_text( ) type 'E'.
  endtry.
endform.

****************************************************
interface lif_abapmerge_marker.
* abapmerge 0.16.7 - 2026-05-06T14:03:43.216Z
  constants c_merge_timestamp type string value `2026-05-06T14:03:43.216Z`.
  constants c_abapmerge_version type string value `0.16.7`.
endinterface.
****************************************************