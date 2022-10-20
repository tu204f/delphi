unit UnitDoubleGridFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts,
  UnitGridFrame;

type
  TDoubleGridFrame = class(TFrame)
    GridPanelLayout: TGridPanelLayout;
    LayoutLeft: TLayout;
    LayoutRight: TLayout;
  private
    GirdFrameLeft: TGirdFrame;
    GirdFrameRight: TGirdFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TDoubleGridFrame }

constructor TDoubleGridFrame.Create(AOwner: TComponent);
var
  xIndComponet: Integer;

  function _InitGridFrame(const AParentLayout: TLayout): TGirdFrame;
  var
    xGirdFrame: TGirdFrame;
  begin
    Inc(xIndComponet);
    xGirdFrame := TGirdFrame.Create(Self);
    xGirdFrame.Parent := AParentLayout;
    xGirdFrame.Align := TAlignLayout.Client;
    xGirdFrame.Name := 'grid_frame_' + xIndComponet.ToString;
    Result := xGirdFrame;
  end;

begin
  inherited Create(AOwner);
  xIndComponet := 0;
  GirdFrameLeft  := _InitGridFrame(LayoutLeft);
  GirdFrameRight := _InitGridFrame(LayoutRight);
end;

destructor TDoubleGridFrame.Destroy;
begin
  FreeAndNil(GirdFrameLeft);
  FreeAndNil(GirdFrameRight);
  inherited;
end;

end.
