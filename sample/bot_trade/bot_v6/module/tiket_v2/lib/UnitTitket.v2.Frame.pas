unit UnitTitket.v2.Frame;

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
  FMX.Layouts,
  FMX.Objects;

type
  TTitketLifeFrame = class(TFrame)
    Layout: TLayout;
    GridPanel: TGridPanelLayout;
    LayoutOpen: TLayout;
    LayoutClose: TLayout;
    RectangleOpen: TRectangle;
    RectangleClose: TRectangle;
    LineOpen: TLine;
    LineClose: TLine;
    LayoutChart: TLayout;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TTitketLifeFrame }

constructor TTitketLifeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TTitketLifeFrame.Destroy;
begin

  inherited;
end;

end.
