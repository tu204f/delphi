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
  Lb.Category,
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
    procedure SetCategory(const Value: TCategory);
    procedure CategoryOnChange(Sender: TObject);
  protected
    procedure SetShowCategory;
  public
    Index: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Category: TCategory read FCategory write SetCategory;
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

procedure TCategoryFrame.SetCategory(const Value: TCategory);
begin
  FCategory := Value;
  SetShowCategory;
  case FCategory.Side of
    tsBuy: Rectangle.Fill.Color := TAlphaColorRec.Green;
    tsSell: Rectangle.Fill.Color := TAlphaColorRec.Red;
  end;
  FCategory.OnChange := CategoryOnChange;
end;

procedure TCategoryFrame.SetShowCategory;
begin
  CheckBoxActive.IsChecked := FCategory.IsActive;
  CheckBoxReActive.IsChecked := FCategory.IsReActive;
  SpinBoxActiveRSI.Value := FCategory.ActiveLevel.Value;
  SpinBoxReActiveRSI.Value := FCategory.ReActiveLevel.Value;
  SpinBoxQty.Value := FCategory.Qty;
end;

procedure TCategoryFrame.CategoryOnChange(Sender: TObject);
begin
  SetShowCategory;
end;

procedure TCategoryFrame.CheckBoxActiveChange(Sender: TObject);
begin
  FCategory.IsActive := CheckBoxActive.IsChecked;
end;

procedure TCategoryFrame.CheckBoxReActiveChange(Sender: TObject);
begin
  FCategory.IsReActive := CheckBoxReActive.IsChecked;
end;

procedure TCategoryFrame.SpinBoxActiveRSIChange(Sender: TObject);
begin
  FCategory.ActiveLevel.Value := SpinBoxActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxReActiveRSIChange(Sender: TObject);
begin
  FCategory.ReActiveLevel.Value := SpinBoxReActiveRSI.Value;
end;

procedure TCategoryFrame.SpinBoxQtyChange(Sender: TObject);
begin
  FCategory.Qty := SpinBoxQty.Value;
end;

end.
