unit UnitCategoryFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Layouts,
  FMX.Objects,
  Lb.Criteria,
  Lb.SysUtils,
  Lb.Level;

type
  ///<summary> атегори€ совершение торговых операций</summary>
  TCategoryFrame = class(TFrame)
    Rectangle: TRectangle;
    GridPanel: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    CheckBoxActive: TCheckBox;
    CheckBoxReActive: TCheckBox;
    SpinBoxActiveRSI: TSpinBox;
    SpinBoxReActiveRSI: TSpinBox;
    SpinBoxQty: TSpinBox;
    procedure CheckBoxActiveChange(Sender: TObject);
    procedure CheckBoxReActiveChange(Sender: TObject);
    procedure SpinBoxActiveRSIChange(Sender: TObject);
    procedure SpinBoxReActiveRSIChange(Sender: TObject);
    procedure SpinBoxQtyChange(Sender: TObject);
  private
    FCriteria: TCriteria;
    procedure SetCriteria(const Value: TCriteria);
    procedure CriteriaOnChange(Sender: TObject);
  protected
    procedure SetShowCriteria;
  public
    Index: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Criteria: TCriteria read FCriteria write SetCriteria;
  end;

implementation

{$R *.fmx}

uses
  Lb.Logger;

{ TCategoryFrame }


constructor TCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCategoryFrame.Destroy;
begin

  inherited;
end;

procedure TCategoryFrame.SetCriteria(const Value: TCriteria);
begin
  FCriteria := Value;
  SetShowCriteria;
  case FCriteria.Side of
    tsBuy: Rectangle.Fill.Color := TAlphaColorRec.Green;
    tsSell: Rectangle.Fill.Color := TAlphaColorRec.Red;
  end;
  FCriteria.OnChange := CriteriaOnChange;
end;

procedure TCategoryFrame.SetShowCriteria;
begin
  CheckBoxActive.IsChecked := FCriteria.IsActive;
  CheckBoxReActive.IsChecked := FCriteria.IsReActive;
  SpinBoxActiveRSI.Value := FCriteria.ActiveLevel.Value;
  SpinBoxReActiveRSI.Value := FCriteria.ReActiveLevel.Value;
  SpinBoxQty.Value := FCriteria.Qty;
end;

procedure TCategoryFrame.CriteriaOnChange(Sender: TObject);
begin
  SetShowCriteria;
end;

procedure TCategoryFrame.CheckBoxActiveChange(Sender: TObject);
begin
  FCriteria.IsActive := CheckBoxActive.IsChecked;
end;

procedure TCategoryFrame.CheckBoxReActiveChange(Sender: TObject);
begin
  FCriteria.IsReActive := CheckBoxReActive.IsChecked;
end;

procedure TCategoryFrame.SpinBoxActiveRSIChange(Sender: TObject);
begin
  FCriteria.ActiveLevel.Value := SpinBoxActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxReActiveRSIChange(Sender: TObject);
begin
  FCriteria.ReActiveLevel.Value := SpinBoxReActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxQtyChange(Sender: TObject);
begin
  FCriteria.Qty := SpinBoxQty.Value;
end;

end.
