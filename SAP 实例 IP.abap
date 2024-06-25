  TYPES: BEGIN OF kernel_version,
           key(21)  TYPE c,
           data(69) TYPE c,
         END OF kernel_version.

  DATA : gt_kernel_version TYPE STANDARD TABLE OF kernel_version,
         gw_kernel_version TYPE kernel_version.

  DATA ip_address(69) TYPE c.
  CALL 'SAPCORE' ID 'ID' FIELD 'VERSION'
                 ID 'TABLE' FIELD gt_kernel_version[].

  READ TABLE gt_kernel_version INTO gw_kernel_version INDEX 11.
  ip_address = gw_kernel_version-data.

  WRITE ip_address.
