{******************************************************************************}
{                                                                              }
{       Delphi cross platform socket library                                   }
{                                                                              }
{       Copyright (c) 2017 WiNDDRiVER(soulawing@gmail.com)                     }
{                                                                              }
{       Homepage: https://github.com/winddriver/Delphi-Cross-Socket            }
{                                                                              }
{******************************************************************************}
unit Net.CrossHttpRouter;

{$I zLib.inc}

interface

uses
  Net.CrossHttpServer;

type
  /// <summary>
  ///   ·��
  /// </summary>
  /// <remarks>
  ///   ���� TCrossHttpServer.Route(), Get(), Post() ��
  /// </remarks>
  TNetCrossRouter = class
  public
    /// <summary>
    ///   ��̬�ļ�·��
    /// </summary>
    /// <param name="ALocalDir">
    ///   ����Ŀ¼
    /// </param>
    class function &Static(const ALocalDir, AFileParamName: string): TCrossHttpRouterProc2; static;

    /// <summary>
    ///   �ļ��б�·��
    /// </summary>
    /// <param name="APath">
    ///   ����·��, �ò�����Ϊ����Ŀ¼�б�ҳ���ж�λ��·��
    /// </param>
    /// <param name="ALocalDir">
    ///   ����Ŀ¼
    /// </param>
    class function Dir(const APath, ALocalDir, ADirParamName: string): TCrossHttpRouterProc2; static;

    /// <summary>
    ///   ����Ĭ����ҳ�ļ��ľ�̬�ļ�·��
    /// </summary>
    /// <param name="ALocalDir">
    ///   ����Ĭ����ҳ�ļ��ı���Ŀ¼
    /// </param>
    /// <param name="ADefIndexFiles">
    ///   Ĭ�ϵ���ҳ�ļ�,��˳��ѡ��,���ҵ��ĸ���ʹ���ĸ�
    /// </param>
    class function Index(const ALocalDir, AFileParamName: string; const ADefIndexFiles: TArray<string>): TCrossHttpRouterProc2; static;
  end;

implementation

uses
  SysUtils,
  Classes,

  Net.CrossHttpRouterDirUtils,
  Net.CrossHttpUtils,

  Utils.IOUtils;

{ TNetCrossRouter }

class function TNetCrossRouter.Index(const ALocalDir, AFileParamName: string;
  const ADefIndexFiles: TArray<string>): TCrossHttpRouterProc2;
var
  LDefIndexFiles: TArray<string>;
begin
  if (ADefIndexFiles <> nil) then
    LDefIndexFiles := ADefIndexFiles
  else
    LDefIndexFiles := [
      'index.html',
      'main.html',
      'index.js',
      'main.js',
      'index.htm',
      'main.htm'
    ];

  Result :=
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LPath, LFile, LDefMainFile: string;
    begin
      LPath := ALocalDir;
      LFile := ARequest.Params[AFileParamName];

      if (LFile = '') then
      begin
        for LDefMainFile in LDefIndexFiles do
        begin
          LFile := TCrossHttpUtils.CombinePath(LPath, LDefMainFile);
          if TFileUtils.Exists(LFile) then
          begin
            AResponse.SendFile(LFile);
            AHandled := True;
            Exit;
          end;
        end;
      end else
      begin
        LFile := TCrossHttpUtils.CombinePath(LPath, LFile);
        if TFileUtils.Exists(LFile) then
        begin
          AResponse.SendFile(LFile);
          AHandled := True;
          Exit;
        end;
      end;

      AHandled := False;
    end;
end;

class function TNetCrossRouter.Static(
  const ALocalDir, AFileParamName: string): TCrossHttpRouterProc2;
begin
  Result :=
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LFile: string;
    begin
      AHandled := True;
      LFile := TCrossHttpUtils.CombinePath(ALocalDir, ARequest.Params[AFileParamName]);
      if (LFile = '') then
      begin
        AHandled := False;
        Exit;
      end;
      LFile := TPathUtils.GetFullPath(LFile);
      AResponse.SendFile(LFile);
    end;
end;

class function TNetCrossRouter.Dir(
  const APath, ALocalDir, ADirParamName: string): TCrossHttpRouterProc2;
begin
  Result :=
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LFile: string;
    begin
      AHandled := True;

      LFile := TCrossHttpUtils.CombinePath(ALocalDir, ARequest.Params[ADirParamName]);
      if (LFile = '') then
      begin
        AHandled := False;
        Exit;
      end;

      LFile := TPathUtils.GetFullPath(LFile);
      if (TDirectoryUtils.Exists(LFile)) then
        AResponse.Send(BuildDirList(LFile, ARequest.Path, APath))
      else if TFileUtils.Exists(LFile) then
        AResponse.SendFile(LFile)
      else
        AHandled := False;
    end;
end;

end.
