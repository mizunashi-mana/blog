from git import Git, Repo
import os


class GitProcess(object):
    def __init__(self, repo: Repo):
        self.repo = repo

    def get_committed_times(self, file_path: str):
        first_commit = None
        last_commit = None

        for commit in self.repo.iter_commits(paths=file_path):
            if last_commit is None:
                last_commit = commit
            first_commit = commit

        return {
            "first_commit_time": first_commit.committed_datetime
            if first_commit
            else None,
            "last_commit_time": last_commit.committed_datetime if last_commit else None,
        }


def build_git_process(repo_path: str) -> GitProcess:
    git = Git()
    git.update_environment(
        GIT_CONFIG_NOSYSTEM="true", HOME=os.getcwd(), XDG_CONFIG_HOME=os.getcwd()
    )
    repo = Repo(repo_path, search_parent_directories=True)

    return GitProcess(repo)
