import click
from crawler.argumentation.convert.common import *
from crawler.argumentation.mine import from_stack
from crawler.argumentation.mine import from_reddit
from crawler.argumentation.mine import from_twitter
from crawler.utils.exceptions.custom_exceptions import *

REDDIT  = 'reddit'
TWITTER = 'twitter'
STACKOVERFLOW = 'stackoverflow'

COMMENTS = 'comments'
USERS    = 'users'
ACCEPTED_MODES = [COMMENTS, USERS]


@click.command()
@click.option(
    '--source',
    '-s',
    prompt='Where do you want to extract argument from \n Sources available twitter | reddit | stackoverflow:',
    help='Argumentation Mining source. Sources available twitter | reddit | stackoverflow')

# REDDIT
@click.option('--submission_id', default=None, help='Reddit Submission ID (you can find it in the URL)')

# TWITTER
@click.option('--query', default=None, help='Twitter Query')

# STACKOVERFLOW
@click.option('--question_id', default=None, help='StackOverflow Question ID (you can find it in the URL)')

# USEFUL PARAMS
@click.option(
    '--mode',
    '-m',
    default='comments',
    prompt='Modes available: comments | users: DEFAULT -> ',
    help='Argumentation Mining method.')
@click.option(
    '--framework',
    '-f',
    default=BWAF,
    prompt='Frameworks available: bwaf | baf | waf | af: DEFAULT ->',
    help='Argumentation Framework.')

# DON'T CARE PARAMS, LEAVE DEFAULT
@click.option('--no_save', is_flag=True, help='If True it saves the results.')
@click.option('--path', '-p', default=None, help='Path where to save the results.')
@click.option('--multiedges', is_flag=True, help='If True, keeps multiedges.')
@click.option('--num_decimal', default=2, help='Number of decimals.')
@click.option('--verbose', '-v', is_flag=True, help='Oh come on.')

def mine_arguments(source, submission_id, query, question_id, mode,
                   framework, no_save, path, multiedges, num_decimal, verbose):
    assert mode in ACCEPTED_MODES, 'NOT VALID MINING METHOD'
    assert framework in ACCEPTED_FRAMEWORKS, 'NOT VALID FRAMEWORK'
    if verbose:
        print('\n', 'START MINING . . .', '\n')
    if source.lower() == REDDIT:
        assert submission_id is not None, 'SUBMISSION ID NOT PRESENT'
        Graph = from_reddit.get_debate_graph(
            submissionId=submission_id,
            mode=mode,
            save=not no_save,
            path=path,
            multiedges=multiedges,
            framework=framework,
            n_decimal=num_decimal,
            verbose=verbose
        )
    elif source.lower() == TWITTER:
        assert query is not None, 'TWITTER QUERY NOT PRESENT'
        Graph = from_twitter.get_debate_graph(
            query=query,
            mode=mode,
            save=not no_save,
            path=path,
            multiedges=multiedges,
            framework=framework,
            n_decimal=num_decimal,
            verbose=verbose
        )
    elif source.lower() == STACKOVERFLOW:
        assert question_id is not None, 'STACKOVERFLOW QUESTION ID NOT PRESENT'
        Graph = from_stack.get_debate_graph(
            question=submission_id,
            mode=mode,
            save=not no_save,
            path=path,
            multiedges=multiedges,
            framework=framework,
            n_decimal=num_decimal,
            verbose=verbose
        )
    else:
        raise(SourceNotValidException('SOURCE NOT VALID'))
    # print(source, submission_id, mode, not no_save, path, multiedges, framework, num_decimal, verbose)
    print('. . . END MINING', '\n')


if __name__ == '__main__':
    mine_arguments()
