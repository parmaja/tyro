# ROADMAP02 smoke suite. Run from the repository root:
#   powershell -ExecutionPolicy Bypass -File tests\run_roadmap02.ps1
# Each case checks the exit code and, where given, a golden file or a stdout
# pattern - so a script that silently stops early cannot pass on exit code alone.
$exe = '.\bin\tyro.exe'
$cases = @(
    @{ Script='tests\roadmap02_p0_api_test.tyro';        Flag='--exit';  Code=0; Golden='roadmap02_p0_api_out.txt';        Pattern='ALL P0-1 API CHECKS PASSED' },
    @{ Script='tests\roadmap02_p0_sound_test.tyro';      Flag='--exit';  Code=0; Stdout='sound duration 2000000000 ms is too long' },
    @{ Script='tests\roadmap02_p0_feedback_test.tyro';   Flag='--exit:2'; Code=0; Stdout='feedback test survived' },
    @{ Script='tests\roadmap02_p0_robustness_test.tyro'; Flag='--exit';  Code=0; Golden='roadmap02_p0_robustness_out.txt'; Pattern='ALL P0-4 ROBUSTNESS CHECKS PASSED' },
    @{ Script='tests\roadmap02_p0_watchdog_test.tyro';   Flag='--exit:5'; Code=1; Stdout='Callback took too long' },
    @{ Script='tests\syntax_error_audit.tyro';           Flag='--exit';  Code=1; Stdout='tyro_script:4: unexpected symbol' },
    @{ Script='tests\test_cli_exit_script.tyro';         Flag='--exit';  Code=0; Stdout='cli-exit-script done' },
    @{ Script='demos\bounce_demo.tyro';                  Flag='--exit:2'; Code=0 },
    @{ Script='demos\animated_demo.tyro';                Flag='--exit:2'; Code=0 },
    @{ Script='demos\sound.tyro';                        Flag='--exit:2'; Code=0 },
    @{ Script='demos\first_game.tyro';                   Flag='--exit:2'; Code=0 },
    @{ Script='tests\roadmap02_p1_scene_test.tyro';      Flag='--exit:3'; Code=0; Golden='roadmap02_p1_scene_out.txt';   Pattern='ALL P1-4 SCENE CHECKS PASSED' },
    @{ Script='tests\roadmap02_p1_pause_test.tyro';      Flag='--exit:3'; Code=0; Golden='roadmap02_p1_pause_out.txt';   Pattern='ALL P1-4 PAUSE CHECKS PASSED' },
    @{ Script='tests\roadmap02_p1_restart_test.tyro';    Flag='--exit:3'; Code=0; Golden='roadmap02_p1_restart_count.txt'; Pattern='2' },
    @{ Script='tests\roadmap02_p1_helpers_test.tyro';    Flag='--exit';  Code=0; Golden='roadmap02_p1_helpers_out.txt';  Pattern='ALL P1-4 HELPER CHECKS PASSED' },
    @{ Script='tests\roadmap02_p2_error_labels_test.tyro'; Flag='--exit:2'; Code=1; Stdout='timer #1:' }
)
$failed = 0
Remove-Item 'tests\roadmap02_p1_restart_count.txt' -ErrorAction SilentlyContinue
foreach ($c in $cases) {
    $text = (& $exe $c.Script $c.Flag 2>&1 | Out-String)
    $code = $LASTEXITCODE
    $ok = ($code -eq $c.Code)
    if ($ok -and $c.Golden) {
        $golden = Join-Path 'tests' $c.Golden
        $ok = (Test-Path $golden) -and ((Get-Content $golden -Raw) -match [regex]::Escape($c.Pattern))
    }
    if ($ok -and $c.Stdout) {
        $ok = ($text -match [regex]::Escape($c.Stdout))
    }
    if ($ok) { $mark = 'PASS' } else { $mark = 'FAIL'; $failed++ }
    "$mark $($c.Script) exit=$code expected=$($c.Code)"
}
# Packaging: --export must produce a runnable folder with a generated fuse.conf.
Remove-Item 'tests\temp_export' -Recurse -Force -ErrorAction SilentlyContinue
& $exe 'games\bubblepop\bubblepop.tyro' '--export=tests\temp_export' '--set-title=Export Probe' 2>&1 | Out-Null
$conf = 'tests\temp_export\fuse.conf'
if ((Test-Path $conf) -and ((Get-Content $conf -Raw) -match 'title=Export Probe')) {
    'PASS --export folder + --set-title'
} else {
    'FAIL --export folder + --set-title'
    $failed++
}
Remove-Item 'tests\temp_export' -Recurse -Force -ErrorAction SilentlyContinue

"BANNER screenshot: bin\tyro.exe tests\roadmap02_p0_banner_test.tyro --exit:2 --screenshot:1"
if ($failed -gt 0) { exit 1 }
'ALL ROADMAP02 SMOKE CHECKS PASSED'
