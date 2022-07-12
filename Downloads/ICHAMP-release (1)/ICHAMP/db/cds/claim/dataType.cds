namespace com.sap.ic.cmh.claim.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Claim.cds ~~~~~~~~~~~~~~~~~~~  */

type FieldControl   : Integer;
type Identifier     : String(30);
type Type           : String(20);
type SupplierRole   : String(10);
type VersionCategory: String(10);
type Quantity       : Decimal(5, 2);
type Unit           : String(5);
type ItemType       : String(20);
type Amount         : Decimal(8, 2);
type Decision       : String(50);
type ContactPerson  : String(50);
type UpdateRestricted : Boolean;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Claim.cds ~~~~~~~~~~~~~~~~~~~~  */