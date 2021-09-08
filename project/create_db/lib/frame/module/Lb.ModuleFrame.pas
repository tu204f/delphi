unit Lb.ModuleFrame;

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
  Lb.Create.DB,
  FMX.Memo.Types,
  FMX.ListBox,
  FMX.Edit,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects;

type
  TModuleFrame = class(TFrame)
    Layout: TLayout;
    LayoutDescription: TLayout;
    TextDescription: TText;
    MemoDescription: TMemo;
    GridPanelLayout: TGridPanelLayout;
    TextID: TText;
    EditID: TEdit;
    TextName: TText;
    EditFieldName: TEdit;
  private
    FModule: TCrModule;
    procedure SetModule(const Value: TCrModule);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Module: TCrModule read FModule write SetModule;
  end;

implementation

{$R *.fmx}

{ TModuleFrame }

constructor TModuleFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModule := nil;
end;

destructor TModuleFrame.Destroy;
begin

  inherited;
end;

procedure TModuleFrame.SetModule(const Value: TCrModule);
begin
  FModule := Value;
end;

end.
