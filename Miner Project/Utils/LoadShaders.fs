module Miner.Utils.LoadShaders

open Pencil.Gaming.Graphics

let loadShaders (vertexPath : string) (fragmentPath : string) =
    let compileAndCheckShader path shaderMode =
        // Compile
        printf "Compiling shader: %s\n" path
        let iden = GL.CreateShader shaderMode
        let code = System.IO.File.ReadAllText path

        GL.ShaderSource (iden, code) // this line is not like the tutorial
        GL.CompileShader iden

        // Check
        let log = GL.GetShaderInfoLog (int iden)
        printf "%s\n" log
        iden
    let vertexShaderID = compileAndCheckShader vertexPath ShaderType.VertexShader
    let fragmentShaderID = compileAndCheckShader fragmentPath ShaderType.FragmentShader

    printf "Linking program\n"
    let programID = GL.CreateProgram ()
    GL.AttachShader (programID, vertexShaderID)
    GL.AttachShader (programID, fragmentShaderID)
    GL.LinkProgram programID

    let log = GL.GetProgramInfoLog (int programID)
    printf "%s\n" log

    GL.DeleteShader vertexShaderID
    GL.DeleteShader fragmentShaderID

    programID

