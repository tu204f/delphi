unit Lb.ModuleUserFrame;

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
  FMX.TabControl,
  FMX.Layouts,
  FMX.Objects,
  Lb.TableTableFrame,
  Lb.MethodTableFrame, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TModuleUserFrame = class(TFrame)
    GridPanelLayout: TGridPanelLayout;
    LayoutTables: TLayout;
    LayoutMethods: TLayout;
    LayoutTablesTop: TLayout;
    LayoutMethodsTop: TLayout;
    Text1: TText;
    Text2: TText;
    LayoutTableTables: TLayout;
    LayoutTableMethods: TLayout;
    Memo: TMemo;
  private
    TableTableFrame: TTableTableFrame;
    MethodTableFrame: TMethodTableFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TModuleUserFrame }

constructor TModuleUserFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TableTableFrame := TTableTableFrame.Create(nil);
  TableTableFrame.Parent := LayoutTableTables;
  TableTableFrame.Align := TAlignLayout.Client;

  MethodTableFrame := TMethodTableFrame.Create(nil);
  MethodTableFrame.Parent := LayoutTableMethods;
  MethodTableFrame.Align := TAlignLayout.Client;
end;

destructor TModuleUserFrame.Destroy;
begin
  FreeAndNil(MethodTableFrame);
  FreeAndNil(TableTableFrame);
  inherited;
end;

end.
