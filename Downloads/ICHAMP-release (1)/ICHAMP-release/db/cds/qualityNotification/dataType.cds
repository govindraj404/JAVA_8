namespace com.sap.ic.cmh.qualityNotification.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Defect.cds ~~~~~~~~~~~~~~~~~~~  */

type Identifier     : String(30);
type SupplierRole   : String(10);
type VersionCategory: String(10);
type ItemType       : String(20);
type Amount         : Decimal(8,2);
type Description    : String(40);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Defect.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF QualityNotification.cds ~~~~~~~~~~~~~~~~~~~  */

type InspectionResult   : String(40);
type ItemNumber         : String(10);
type Type               : String(20);
type Quantity           : Decimal(5, 2);
type Unit               : String(5);
type ContactPerson      : String(50);
type Number             : String(30);
type Role               : String(2);
type FieldControl       : Integer;
type UpdateRestricted   : Boolean;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF QualityNotification.cds ~~~~~~~~~~~~~~~~~~~~  */