unit UnitCategoryFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Layouts;

type
  ///<summary> атегори€ совершение торговых операций</summary>
  TCategoryFrame = class(TFrame)
    GridPanelLayout: TGridPanelLayout;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

end.
