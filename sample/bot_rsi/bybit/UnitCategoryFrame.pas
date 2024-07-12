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
  Lb.SysUtils,
  Lb.Trader;

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
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValueRSI(const AValueRSI: Double);
    procedure LoadParam;
    procedure SaveParam;
  end;

implementation

{$R *.fmx}



{ TCategoryFrame }

constructor TCategoryFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TCategoryFrame.Destroy;
begin

  inherited;
end;

procedure TCategoryFrame.SetValueRSI(const AValueRSI: Double);
begin

end;

procedure TCategoryFrame.LoadParam;
begin

end;

procedure TCategoryFrame.SaveParam;
begin

end;

end.
