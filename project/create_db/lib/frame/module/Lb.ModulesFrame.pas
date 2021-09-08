unit Lb.ModulesFrame;

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  Lb.Create.DB;

type
  ///<summary>Список модулей</summary>
  TModuleTableFrame = class(TFrame)
    StringGrid: TStringGrid;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    LayoutMenu: TLayout;
    Layout: TLayout;
  private
    FModules: TCrModules;
    procedure SetModules(const Value: TCrModules);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Modules: TCrModules read FModules write SetModules;
  end;

implementation

{$R *.fmx}

{ TModulesFrame }

constructor TModuleTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModules := nil;
end;

destructor TModuleTableFrame.Destroy;
begin

  inherited;
end;

procedure TModuleTableFrame.SetModules(const Value: TCrModules);
begin
  FModules := Value;
end;

end.
