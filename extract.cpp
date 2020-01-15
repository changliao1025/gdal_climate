#pragma once
#include "extract.h"

int extract::extract()
{}
int extract::~extract()
{}

int extract::extract_raster_by_mask(
                                    extension_file = ef, $
                                    filename_mapinfo = map, $
                                    missing_value, $
                                    ncolumn = nc, $
                                    nrow = nr, $
                                    prefix_in, $
                                    prefix_out =  po, $
                                    shapefile_in, $
                                    workspace_in, $
                                    workspace_out, $
                                    year_end, $
                                    year_start


                                    )
{
  int error_code =1;

  ;;shapefile is required for extraction
      IF (FILE_TEST(shapefile_in) != 1)
	{

	  }
        map_info_flag = 1
        IF KEYWORD_SET(map) THEN BEGIN
        IF FILE_TEST(map) THEN BEGIN
        ENVI_OPEN_FILE, map, r_fid = fid
        IF fid NE -1 THEN BEGIN
        map_info = ENVI_GET_MAP_INFO(FID = fid)
        ENVI_FILE_MNG, id = fid, /remove
        ENDIF ELSE BEGIN
        PRINT, 'The map info file is corrupted!'
        map_info_flag = 0
        ENDELSE
        ENDIF ELSE BEGIN
        PRINT, 'The map info file does not exist!'
        map_info_flag = 0
        ENDELSE
        ENDIF ELSE BEGIN
        ;;if there is no map info, the default will be used?
                     map_info_flag = 0
                     ENDELSE

                     IF KEYWORD_SET(ef) THEN BEGIN
                     extension_file = ef
                     ENDIF ELSE BEGIN
                     extension_file = extension_envi
                     ENDELSE
                     IF KEYWORD_SET(nc) THEN BEGIN
                     ncolumn = nc
                     ENDIF ELSE BEGIN
                     RETURN
                     ENDELSE
                     IF KEYWORD_SET(nr) THEN BEGIN
                     nrow = nr
                     ENDIF ELSE BEGIN
                     RETURN
                     ENDELSE


                     IF KEYWORD_SET(po) THEN BEGIN
                     prefix_out =  po
                     ENDIF ELSE BEGIN
                     prefix_out = prefix_in
                     ENDELSE

                     ;;==================================================
            FOR year = year_start, year_end, 1 DO BEGIN
              year_str = STRING(year, format = '(I04)')
              year_in = workspace_in + !slash + year_str
              year_out = workspace_out + !slash + year_str
              FOR day = 0, 382, 1 DO BEGIN
              day_str = STRING(day, format = '(I03)')
              filename_in = year_in + !slash + prefix_in + year_str + day_str + extension_file
              IF FILE_TEST(filename_in) EQ 1 THEN BEGIN
              IF FILE_TEST(year_out) NE 1 THEN BEGIN
              FILE_MKDIR, year_out
              ENDIF
              ENVI_OPEN_FILE, filename_in, r_fid = fid_in
              IF fid_in NE -1 THEN BEGIN
              ENVI_FILE_QUERY, fid_in, ns = ns_in, nl = nl_in, nb = nb_in, bname = bname_in, dims = dims_in
              pos = LINDGEN(nb_in)
              ;;===================================================================
              ;;Read shapefile
                  oshp = OBJ_NEW('IDLffshape', shapefile_in)
                  oshp -> GetProperty, n_entities = n_ent, Attribute_info = attr_info, $
                  n_attributes = n_attr, Entity_type = ent_type
                  roi_shp = LONARR(n_ent)
                  FOR ishp = 0, n_ent - 1 DO BEGIN
                  entitie = oshp -> GetEntity(ishp)
                  ;;Check polygon
                      IF entitie.SHAPE_TYPE EQ 5 THEN BEGIN
                      record = *(entitie.VERTICES)
                      ;;Convert coordinates
                          ENVI_CONVERT_FILE_COORDINATES, fid_in, xmap, ymap, record[0, *], record[1, *]
                          ;;Create ROI
                              roi_shp[ishp] = ENVI_CREATE_ROI(ns = ns_in, nl = nl_in)
                              ENVI_DEFINE_ROI, roi_shp[ishp], /polygon, xpts = REFORM(xmap), ypts = REFORM(ymap)
                              IF ishp EQ 0 THEN BEGIN
                              ;;nearest sampling is used
                                  xmin = ROUND(MIN(xMap))
                                  yMin = ROUND(MIN(yMap))
                                  ENDIF ELSE BEGIN
                                  ;;there should be only one polygon
                                      RETURN
                                      ENDELSE
                                      ENDIF
                                      oshp -> DestroyEntity, entitie
                                      ENDFOR
                                      ;;===============================================================
                                      ENVI_MASK_DOIT, /IN_MEMORY, /inside, $
                                        AND_OR = 1, $
                                        ns = ns_in, nl = nl_in, $
                                        r_fid = fid_mask, ROI_IDS = roi_shp

                                        dims_mask = [-1, xMin, (xMin + ncolumn - 1), yMin, (ymin + nrow - 1)]
                                        m_pos = [0]

                                        filename_out = year_out + !slash + prefix_out + year_str + day_str + extension_envi
                                        ;;==================================================================
                                        IF  map_info_flag EQ 1 THEN BEGIN
                                          ENVI_MASK_APPLY_DOIT, /in_memory, DIMS = dims_mask, $
                                          FID = fid_in, $
                                          M_FID = fid_mask, M_POS = m_pos, $
                                          POS = pos, $
                                          R_FID = fid_out, $
                                          VALUE = missing_value

                                          ENVI_FILE_QUERY, bname = bname_out, $
                                          dims = dims_out, $
                                          fid_out, $
                                          ns = ns_out, nl = nl_out, nb = nb_out

                                          data = ENVI_GET_DATA(fid = fid_out, dims = dims_out, pos = pos)

                                          ENVI_WRITE_ENVI_FILE, FLOAT(data), $
                                          map_info = map_info, $
                                          nb = nb_out, ns = ncolumn, nl = nrow, $
                                          OUT_DT = 4, out_name = filename_out

                                          ENDIF ELSE BEGIN
                                          ENVI_MASK_APPLY_DOIT,  DIMS = dims_mask, $
                                          FID = fid_in, $
                                          M_FID = fid_mask, M_POS = m_pos, $
                                          OUT_NAME = filename_out, $
                                          POS = pos, $
                                          R_FID = fid_out, $
                                          VALUE = missing_value

                                          ENDELSE

                                          ENVI_FILE_MNG, id = fid_in, /remove
                                          ENVI_FILE_MNG, id = fid_mask, /remove
                                          ENVI_FILE_MNG, id = fid_out, /remove
                                          ENDIF ELSE BEGIN
                                          PRINT, filename_in
                                          PRINT, 'Cannot open the file!'
                                          ENDELSE
                                          ENDIF else begin
                                                       print, 'File does not exist: ' + filename_in
                                                       endelse
                                                       ENDFOR                     ;end day loop
                                                                                     print, year_str + ' is finished!'
                                                                                     ENDFOR                        ;end year loop
                                                                                                                      PRINT, 'Finished!'




                                                                                                                      return error_code;

}
