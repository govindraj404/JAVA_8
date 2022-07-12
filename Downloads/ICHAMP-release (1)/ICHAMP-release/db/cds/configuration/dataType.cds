namespace com.sap.ic.cmh.configuration.dataType;

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Business Object Attribute.cds ~~~~~~~~~~~~~~~~~~~  */

type BusinessObjectAttribute : String(100);
type BusinessObjectType : String(40);
type Name : String(60);
/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Business Object Attribute.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Business Object Configuration.cds ~~~~~~~~~~~~~~~~~~~  */

type BusinessObjectValue : String(100);
/* ~~~~~~~~~~~~~~~~~~~~~~ END OF Business Object Configuration.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Condition Type.cds ~~~~~~~~~~~~~~~~~~~  */

type Identifier : Integer;
type Destination : String(30);
type ConditionType : String(20);
type Description : String(40);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Condition Type.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Service Material.cds ~~~~~~~~~~~~~~~~~~~  */

type ServiceMaterial : String(40);

/* ~~~~~~~~~~~~~~~~~~~~~~~~~ END OF Service Material.cds ~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF ServiceMaterialUnit.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */

type Numerator : Decimal(4, 0);
type Denominator : Decimal(4, 0);
type Flag : Boolean;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ END OF ServiceMaterialUnit.cds ~~~~~~~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Complaint Type.cds ~~~~~~~~~~~~~~~~~~~  */

type Code : String(10);
type TargetSystem : String(10);
/* ~~~~~~~~~~~~~~~~~~~~~~ END OF Complaint Type.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Reference Type.cds ~~~~~~~~~~~~~~~~~~~  */

type ReferenceTypeCode : String(4);

/* ~~~~~~~~~~~~~~~~~~~~~~ END OF Reference Type.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Target Type.cds ~~~~~~~~~~~~~~~~~~~  */

type TargetTypeCode : String(4);
/* ~~~~~~~~~~~~~~~~~~~~~~ END OF Target Type.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Refund control.cds ~~~~~~~~~~~~~~~~~~~  */

type RefundControlCode : String(1);
type RefundControlDescription : String(60);
/* ~~~~~~~~~~~~~~~~~~~~~~ END OF Refund Control.cds ~~~~~~~~~~~~~~~~~~~  */

/* ~~~~~~~~~~~~~~~~~~~~~~ START OF Item Category.cds ~~~~~~~~~~~~~~~~~~~  */

type ConditionType_ItemCategory : String(4);
/* ~~~~~~~~~~~~~~~~~~~~~~ END OF Item Category.cds ~~~~~~~~~~~~~~~~~~~  */

